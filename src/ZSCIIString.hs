module ZSCIIString 
where 

import MemoryMap
import Data.Word(Word16)
import Data.Word(Word8)
import Data.Bits
import Data.Char


-- ZChars are 5 bits, but will store in 8 bit words.
type ZChar = Word8


-- ZChar interpretation will depend upon the current shift register.
data ShiftRegister = UPPER | LOWER | SYMBOL

-- ZSCII Strings pack three 5-bit characters into a 16-bit word. The string terminator is the first bit of the WORD16. If it is 1, the string is terminated. So to read a full ZSCII string, we have to read until we find a character with a most significant bit true. 
readZSCIIString :: MemoryMap -> Location -> [MemoryCell]
readZSCIIString current loc =   let cell = readMemoryCell current loc
				in case (cell) of 
				Nothing -> []
				Just cell -> case (testBit cell 15 ) of
						True -> [cell]
						False -> cell : readZSCIIString current (loc+1)




--

splitMemoryCellToZChar :: MemoryCell -> [ZChar] 
splitMemoryCellToZChar cell = let mod_cell = clearBit cell 15 -- need to clear the bit that serves as new-line indicator
				  zchar1 = fromIntegral (shiftR mod_cell 10) :: Word8
				  zchar2 = fromIntegral (shiftR (shiftL mod_cell 5) 10) :: Word8
				  zchar3 = fromIntegral (shiftR (shiftL mod_cell 10) 10) :: Word8
			      in [zchar1,  zchar2,  zchar3]


--convertZSCIIStringToZCharString :: ShiftRegister -> Memory -> [ZChar]

--

convertZCharToASCIIChar :: ShiftRegister -> ZChar  -> Char
convertZCharToASCIIChar LOWER zchar = chr ((fromIntegral zchar) + 91)
convertZCharToASCIIChar UPPER zchar = chr ((fromIntegral zchar) + 59)
convertZCharToASCIIChar SYMBOL 6 = ' '
convertZCharToASCIIChar SYMBOL 7 = '0'
convertZCharToASCIIChar SYMBOL 8 = '1'
convertZCharToASCIIChar SYMBOL 9 = '2'
convertZCharToASCIIChar SYMBOL 10 = '3'
convertZCharToASCIIChar SYMBOL 11 = '4'
convertZCharToASCIIChar SYMBOL 12 = '5'
convertZCharToASCIIChar SYMBOL 13 = '6'
convertZCharToASCIIChar SYMBOL 14 = '7'
convertZCharToASCIIChar SYMBOL 15 = '8'
convertZCharToASCIIChar SYMBOL 16 = '9'
convertZCharToASCIIChar SYMBOL 17 = '.'
convertZCharToASCIIChar SYMBOL 18 = ','
convertZCharToASCIIChar SYMBOL 19 = '!'
convertZCharToASCIIChar SYMBOL 20 = '?'
convertZCharToASCIIChar SYMBOL 21 = '_'
convertZCharToASCIIChar SYMBOL 22 = '#'
convertZCharToASCIIChar SYMBOL 23 = '\''
convertZCharToASCIIChar SYMBOL 24 = '"'
convertZCharToASCIIChar SYMBOL 25 = '/'
convertZCharToASCIIChar SYMBOL 26 = '\\'
convertZCharToASCIIChar SYMBOL 27 = '<'
convertZCharToASCIIChar SYMBOL 28 = '-'
convertZCharToASCIIChar SYMBOL 29 = ':'
convertZCharToASCIIChar SYMBOL 30 = '('
convertZCharToASCIIChar SYMBOL 31 = ')'



