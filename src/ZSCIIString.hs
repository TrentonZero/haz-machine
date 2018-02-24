module ZSCIIString where

import           Data.Bits
import           Data.Char
import           Data.Maybe
import           Data.Word   (Word16, Word8)
import           Debug.Trace
import qualified MemoryMap   as MM
{-# ANN module ("HLint: ignore Redundant bracket"::String) #-}


-- ZChars are 5 bits, but will store in 8 bit words.
type ZChar = Word8

-- return an ascii string
readASCIIString
   :: MM.MemoryMap -> MM.Location -> String
readASCIIString current loc =
  let zscii_mc = readZSCIIString current loc
      zscii = concatMap splitMemoryCellToZChar zscii_mc
      ascii = catMaybes $ evaluateZString current zscii
  in ascii


-- ZSCII Strings pack three 5-bit characters into a 16-bit word. The string terminator is the first bit of the WORD16. If it is 1, the string is terminated. So to read a full ZSCII string, we have to read until we find a character with a most significant bit true.
readZSCIIString
  :: MM.MemoryMap -> MM.Location -> [MM.MemoryCell]
readZSCIIString current loc =
  let cell = MM.readMemoryCell current loc
  in case cell of
       Nothing -> []
       Just cell ->
         if (testBit cell 15) then [cell] else
           cell :
           readZSCIIString current
                           (loc + 1)

--
splitMemoryCellToZChar :: MM.MemoryCell -> [ZChar]
splitMemoryCellToZChar cell =
  let mod_cell = (.&.) cell 0x7FFF -- need to clear the bit that serves as new-line indicator
      zchar1 = fromIntegral ( shiftR ((.&.) mod_cell 0x7C00) 10) :: Word8
      zchar2 = fromIntegral ( shiftR ((.&.) mod_cell 0x3E0 ) 5) :: Word8
      zchar3 = fromIntegral (  (.&.) mod_cell 0x1F  ) :: Word8
  in [zchar1,zchar2,zchar3]

appendF :: Word16 -> Word16 -> Word16
appendF cur char =
  let result =(.|.) (shiftL cur 5)  char
  in trace("calling appendF with cur:" ++ show cur ++ " char:" ++ show char ++ " and result:" ++ show result) result

assembleMemoryCell :: [Word16] -> Word16
assembleMemoryCell xs =
  let myset = take 3 xs
      cell = foldl appendF 0 myset
  in trace("calling assembleMemoryCell with values: " ++ show xs ++ " and result: " ++ show cell) cell

lineTermMemoryCell :: Word16 -> Word16
lineTermMemoryCell cell = setBit cell 15


-- convertZSCIIStringToZCharString :: MM.ShiftRegister -> Memory -> [ZChar]
convertZCharToASCIICharGivenState'
  :: (MM.MemoryMap,Maybe Char) -> ZChar -> (MM.MemoryMap,Maybe Char)
convertZCharToASCIICharGivenState' (state, shift) zchar = trace ("calling convertZCharToASCIICharGivenState with state:" ++ show state ++ " shift: " ++ show shift ++ " zchar:" ++ show zchar ++ " result:" ++ show (convertZCharToASCIICharGivenState (state, shift) zchar)) (convertZCharToASCIICharGivenState (state, shift) zchar)
convertZCharToASCIICharGivenState
  :: (MM.MemoryMap,Maybe Char) -> ZChar -> (MM.MemoryMap,Maybe Char)
convertZCharToASCIICharGivenState (state,_) zchar =
  let shiftR = MM.shiftRegister state
      newShiftAndChar = convertZCharToASCIIChar shiftR zchar
  in (MM.updateShiftRegister state $ fst newShiftAndChar, snd newShiftAndChar)

convertZCharToASCIIChar
  :: MM.ShiftRegister -> ZChar -> (MM.ShiftRegister,Maybe Char)
convertZCharToASCIIChar MM.LOWER zchar
  | zchar >= 6 = (MM.LOWER,Just (chr (fromIntegral zchar + 91)))
convertZCharToASCIIChar MM.LOWER 2 = (MM.UPPER_THEN_LOWER,Nothing)
convertZCharToASCIIChar MM.LOWER 3 = (MM.SYMBOL_THEN_LOWER,Nothing)
convertZCharToASCIIChar MM.LOWER 4 = (MM.UPPER,Nothing)
convertZCharToASCIIChar MM.LOWER 5 = (MM.SYMBOL,Nothing)
convertZCharToASCIIChar MM.UPPER zchar
  | zchar >= 6 = (MM.UPPER,Just (chr (fromIntegral zchar + 59)))
convertZCharToASCIIChar MM.UPPER 2 = (MM.SYMBOL_THEN_UPPER,Nothing)
convertZCharToASCIIChar MM.UPPER 3 = (MM.LOWER_THEN_UPPER,Nothing)
convertZCharToASCIIChar MM.UPPER 4 = (MM.SYMBOL,Nothing)
convertZCharToASCIIChar MM.UPPER 5 = (MM.LOWER,Nothing)
convertZCharToASCIIChar MM.SYMBOL 2 = (MM.LOWER_THEN_SYMBOL,Nothing)
convertZCharToASCIIChar MM.SYMBOL 3 = (MM.UPPER_THEN_SYMBOL,Nothing)
convertZCharToASCIIChar MM.SYMBOL 4 = (MM.SYMBOL,Nothing)
convertZCharToASCIIChar MM.SYMBOL 5 = (MM.LOWER,Nothing)
convertZCharToASCIIChar MM.SYMBOL 6 = (MM.SYMBOL,Just ' ')
convertZCharToASCIIChar MM.SYMBOL 7 = (MM.SYMBOL,Just '0')
convertZCharToASCIIChar MM.SYMBOL 8 = (MM.SYMBOL,Just '1')
convertZCharToASCIIChar MM.SYMBOL 9 = (MM.SYMBOL,Just '2')
convertZCharToASCIIChar MM.SYMBOL 10 = (MM.SYMBOL,Just '3')
convertZCharToASCIIChar MM.SYMBOL 11 = (MM.SYMBOL,Just '4')
convertZCharToASCIIChar MM.SYMBOL 12 = (MM.SYMBOL,Just '5')
convertZCharToASCIIChar MM.SYMBOL 13 = (MM.SYMBOL,Just '6')
convertZCharToASCIIChar MM.SYMBOL 14 = (MM.SYMBOL,Just '7')
convertZCharToASCIIChar MM.SYMBOL 15 = (MM.SYMBOL,Just '8')
convertZCharToASCIIChar MM.SYMBOL 16 = (MM.SYMBOL,Just '9')
convertZCharToASCIIChar MM.SYMBOL 17 = (MM.SYMBOL,Just '.')
convertZCharToASCIIChar MM.SYMBOL 18 = (MM.SYMBOL,Just ',')
convertZCharToASCIIChar MM.SYMBOL 19 = (MM.SYMBOL,Just '!')
convertZCharToASCIIChar MM.SYMBOL 20 = (MM.SYMBOL,Just '?')
convertZCharToASCIIChar MM.SYMBOL 21 = (MM.SYMBOL,Just '_')
convertZCharToASCIIChar MM.SYMBOL 22 = (MM.SYMBOL,Just '#')
convertZCharToASCIIChar MM.SYMBOL 23 = (MM.SYMBOL,Just '\'')
convertZCharToASCIIChar MM.SYMBOL 24 = (MM.SYMBOL,Just '"')
convertZCharToASCIIChar MM.SYMBOL 25 = (MM.SYMBOL,Just '/')
convertZCharToASCIIChar MM.SYMBOL 26 = (MM.SYMBOL,Just '\\')
convertZCharToASCIIChar MM.SYMBOL 27 = (MM.SYMBOL,Just '<')
convertZCharToASCIIChar MM.SYMBOL 28 = (MM.SYMBOL,Just '-')
convertZCharToASCIIChar MM.SYMBOL 29 = (MM.SYMBOL,Just ':')
convertZCharToASCIIChar MM.SYMBOL 30 = (MM.SYMBOL,Just '(')
convertZCharToASCIIChar MM.SYMBOL 31 = (MM.SYMBOL,Just ')')
convertZCharToASCIIChar MM.UPPER_THEN_LOWER zchar = (MM.LOWER,snd (convertZCharToASCIIChar MM.UPPER zchar))
convertZCharToASCIIChar MM.LOWER_THEN_UPPER zchar = (MM.UPPER,snd (convertZCharToASCIIChar MM.LOWER zchar))
convertZCharToASCIIChar MM.LOWER_THEN_SYMBOL zchar = (MM.SYMBOL,snd (convertZCharToASCIIChar MM.LOWER zchar))
convertZCharToASCIIChar MM.UPPER_THEN_SYMBOL zchar = (MM.SYMBOL,snd (convertZCharToASCIIChar MM.UPPER zchar))
convertZCharToASCIIChar MM.SYMBOL_THEN_LOWER zchar = (MM.LOWER,snd (convertZCharToASCIIChar MM.SYMBOL zchar))
convertZCharToASCIIChar MM.SYMBOL_THEN_UPPER zchar = (MM.UPPER,snd (convertZCharToASCIIChar MM.SYMBOL zchar))


--- evaluateZString state zchar = let x = (map snd (map (convertZCharToASCIICharGivenState state) zchar))
---				in if (x == Nothing) map fromJust
evaluateZString' state zchar = trace ("calling evaluateZString with state:" ++ show state ++ " zchar:" ++ show zchar ++ " result: "  ++ show (evaluateZString state zchar)) (evaluateZString state zchar)
evaluateZString :: MM.MemoryMap -> [ZChar] -> [Maybe Char]
evaluateZString state [] = error "empty zstring"
evaluateZString state [x] = [snd (convertZCharToASCIICharGivenState (state, Nothing) x)]
evaluateZString state (x:xs) =
  let newstate = fst (convertZCharToASCIICharGivenState (state, Nothing) x)
  in evaluateZString state [x] ++ evaluateZString newstate xs
