module ZSCIIString 
(
    ZSCIIString.tests
)
where 

import MemoryMap
import Data.Word(Word16)
import Data.Bits
import Distribution.TestSuite

-- ZSCII Strings pack three 5-bit characters into a 16-bit word. The string terminator is the first bit of the WORD16. If it is 1, the string is terminated. So to read a full ZSCII string, we have to read until we find a character with a most significant bit true. 
readZSCIIString :: Memory -> Location -> Memory
readZSCIIString current loc =   let cell = readMemoryCell current loc
				in case (cell) of 
				Nothing -> []
				Just cell -> case (testBit cell 15 ) of
						True -> [cell]
						False -> cell : readZSCIIString current (loc+1)

test_readZSCIIString_base = let memory = [1,2,3,4,5,6,7,0xFFFF,8,9,10]
				location = 2
				expected_memory =[3,4,5,6,7,0xFFFF] 
			    in assertWithMessage (readZSCIIString memory location == expected_memory) "Did not correctly read ZCSII string from memory"

test_readZSCIIString_noterm = let   memory = [1,2,3,4,5,6,7,8,9,10]
				    location = 2
				    expected_memory =[3,4,5,6,7,8,9,10] 
			    in assertWithMessage (readZSCIIString memory location == expected_memory) "Did not correctly read ZCSII string from memory"

--------- TEST CASES ----------

assertWithMessage :: Bool ->  String -> Progress
assertWithMessage condition fail_message =
		 if condition then 
		    Finished Pass
		else
		    Finished $ Fail fail_message

assert :: Bool -> Progress
assert condition = assertWithMessage condition "no match"



tests :: IO [Test]
tests = return [
		    Test t_readZSCIIString_base,
		    Test t_readZSCIIString_noterm
		]
    where
	t_readZSCIIString_noterm = TestInstance
	    { run = return $ test_readZSCIIString_noterm
		    , name = "test_readZSCIIString_noterm"
		    , tags = []
		    , options = []
		    , setOption = \_ _ -> Right t_readZSCIIString_noterm
	    }
	t_readZSCIIString_base = TestInstance
	    { run = return $ test_readZSCIIString_base
		    , name = "test_readZSCIIString_base"
		    , tags = []
		    , options = []
		    , setOption = \_ _ -> Right t_readZSCIIString_base
	    }







