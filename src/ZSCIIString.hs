module ZSCIIString where

import MemoryMap
import Data.Word (Word16)
import Data.Word (Word8)
import Data.Bits
import Data.Char
import Debug.Trace

-- ZChars are 5 bits, but will store in 8 bit words.
type ZChar = Word8

-- ZSCII Strings pack three 5-bit characters into a 16-bit word. The string terminator is the first bit of the WORD16. If it is 1, the string is terminated. So to read a full ZSCII string, we have to read until we find a character with a most significant bit true. 
readZSCIIString
  :: MemoryMap -> Location -> [MemoryCell]
readZSCIIString current loc = 
  let cell = readMemoryCell current loc
  in case (cell) of
       Nothing -> []
       Just cell -> 
         case (testBit cell 15) of
           True -> [cell]
           False -> 
             cell :
             readZSCIIString current
                             (loc + 1)

--
splitMemoryCellToZChar :: MemoryCell -> [ZChar]
splitMemoryCellToZChar cell = 
  let mod_cell = clearBit cell 15 -- need to clear the bit that serves as new-line indicator
      zchar1 = fromIntegral (shiftR mod_cell 10) :: Word8
      zchar2 = fromIntegral (shiftR (shiftL mod_cell 5) 10) :: Word8
      zchar3 = fromIntegral (shiftR (shiftL mod_cell 10) 10) :: Word8
  in [zchar1,zchar2,zchar3]

--convertZSCIIStringToZCharString :: ShiftRegister -> Memory -> [ZChar]
--
convertZCharToASCIICharGivenState'
  :: (MemoryMap,Maybe Char) -> ZChar -> (MemoryMap,Maybe Char)
convertZCharToASCIICharGivenState' (state, shift) zchar = trace ("calling convertZCharToASCIICharGivenState with state:" Prelude.++ show state Prelude.++ " shift: " Prelude.++ show shift Prelude.++ " zchar:" Prelude.++ show zchar Prelude.++ " result:" Prelude.++ show (convertZCharToASCIICharGivenState (state, shift) zchar)) (convertZCharToASCIICharGivenState (state, shift) zchar)
convertZCharToASCIICharGivenState
  :: (MemoryMap,Maybe Char) -> ZChar -> (MemoryMap,Maybe Char)
convertZCharToASCIICharGivenState (state,_) zchar = 
  let shiftR = (shiftRegister state)
      newShiftAndChar = convertZCharToASCIIChar shiftR zchar
  in ((updateShiftRegister state $ fst newShiftAndChar), (snd newShiftAndChar))

convertZCharToASCIIChar
  :: ShiftRegister -> ZChar -> (ShiftRegister,Maybe Char)
convertZCharToASCIIChar LOWER zchar
  | zchar >= 6 = (LOWER,Just (chr ((fromIntegral zchar) + 91)))
convertZCharToASCIIChar LOWER 2 = (UPPER_THEN_LOWER,Nothing)
convertZCharToASCIIChar LOWER 3 = (SYMBOL_THEN_LOWER,Nothing)
convertZCharToASCIIChar LOWER 4 = (UPPER,Nothing)
convertZCharToASCIIChar LOWER 5 = (SYMBOL,Nothing)
convertZCharToASCIIChar UPPER zchar
  | zchar >= 6 = (UPPER,Just (chr ((fromIntegral zchar) + 59)))
convertZCharToASCIIChar UPPER 2 = (SYMBOL_THEN_UPPER,Nothing)
convertZCharToASCIIChar UPPER 3 = (LOWER_THEN_UPPER,Nothing)
convertZCharToASCIIChar UPPER 4 = (SYMBOL,Nothing)
convertZCharToASCIIChar UPPER 5 = (LOWER,Nothing)
convertZCharToASCIIChar SYMBOL 2 = (LOWER_THEN_SYMBOL,Nothing)
convertZCharToASCIIChar SYMBOL 3 = (UPPER_THEN_SYMBOL,Nothing)
convertZCharToASCIIChar SYMBOL 4 = (SYMBOL,Nothing)
convertZCharToASCIIChar SYMBOL 5 = (LOWER,Nothing)
convertZCharToASCIIChar SYMBOL 6 = (SYMBOL,Just ' ')
convertZCharToASCIIChar SYMBOL 7 = (SYMBOL,Just '0')
convertZCharToASCIIChar SYMBOL 8 = (SYMBOL,Just '1')
convertZCharToASCIIChar SYMBOL 9 = (SYMBOL,Just '2')
convertZCharToASCIIChar SYMBOL 10 = (SYMBOL,Just '3')
convertZCharToASCIIChar SYMBOL 11 = (SYMBOL,Just '4')
convertZCharToASCIIChar SYMBOL 12 = (SYMBOL,Just '5')
convertZCharToASCIIChar SYMBOL 13 = (SYMBOL,Just '6')
convertZCharToASCIIChar SYMBOL 14 = (SYMBOL,Just '7')
convertZCharToASCIIChar SYMBOL 15 = (SYMBOL,Just '8')
convertZCharToASCIIChar SYMBOL 16 = (SYMBOL,Just '9')
convertZCharToASCIIChar SYMBOL 17 = (SYMBOL,Just '.')
convertZCharToASCIIChar SYMBOL 18 = (SYMBOL,Just ',')
convertZCharToASCIIChar SYMBOL 19 = (SYMBOL,Just '!')
convertZCharToASCIIChar SYMBOL 20 = (SYMBOL,Just '?')
convertZCharToASCIIChar SYMBOL 21 = (SYMBOL,Just '_')
convertZCharToASCIIChar SYMBOL 22 = (SYMBOL,Just '#')
convertZCharToASCIIChar SYMBOL 23 = (SYMBOL,Just '\'')
convertZCharToASCIIChar SYMBOL 24 = (SYMBOL,Just '"')
convertZCharToASCIIChar SYMBOL 25 = (SYMBOL,Just '/')
convertZCharToASCIIChar SYMBOL 26 = (SYMBOL,Just '\\')
convertZCharToASCIIChar SYMBOL 27 = (SYMBOL,Just '<')
convertZCharToASCIIChar SYMBOL 28 = (SYMBOL,Just '-')
convertZCharToASCIIChar SYMBOL 29 = (SYMBOL,Just ':')
convertZCharToASCIIChar SYMBOL 30 = (SYMBOL,Just '(')
convertZCharToASCIIChar SYMBOL 31 = (SYMBOL,Just ')')
convertZCharToASCIIChar UPPER_THEN_LOWER zchar = (LOWER,snd (convertZCharToASCIIChar UPPER zchar))
convertZCharToASCIIChar LOWER_THEN_UPPER zchar = (UPPER,snd (convertZCharToASCIIChar LOWER zchar))
convertZCharToASCIIChar LOWER_THEN_SYMBOL zchar = (SYMBOL,snd (convertZCharToASCIIChar LOWER zchar))
convertZCharToASCIIChar UPPER_THEN_SYMBOL zchar = (SYMBOL,snd (convertZCharToASCIIChar UPPER zchar))
convertZCharToASCIIChar SYMBOL_THEN_LOWER zchar = (LOWER,snd (convertZCharToASCIIChar SYMBOL zchar))
convertZCharToASCIIChar SYMBOL_THEN_UPPER zchar = (UPPER,snd (convertZCharToASCIIChar SYMBOL zchar))

