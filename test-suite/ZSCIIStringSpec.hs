-- test-suite/ZSCIIStringSpec.hs
module ZSCIIStringSpec (spec) where
import ZSCIIString
import MemoryMap
import Test.Hspec
import Test.Hspec.QuickCheck
import Data.Vector.Unboxed

spec :: Spec
spec = parallel $ do 
    describe "ZSCIIString Tests" $ do
	test_readZSCIIString_base
	test_readZSCIIString_noterm
	test_splitMemoryCellToZChar
	test_splitMemoryCellToZChar_with_term
	test_convertZCharToASCIIChar_lower
	test_convertZCharToASCIIChar_upper
	test_convertZCharToASCIIChar_symbol



test_readZSCIIString_base = let memory = MemoryMap (fromList [1,2,3,4,5,6,7,0xFFFF,8,9,10]) [] 0 []
				location = 2
				expected_memory =[3,4,5,6,7,0xFFFF] 
			    in assertWithMessage (readZSCIIString memory location == expected_memory) "Read ZCSII string from memory"



test_readZSCIIString_noterm = let   memory = MemoryMap (fromList [1,2,3,4,5,6,7,8,9,10]) [] 0 []
				    location = 2
				    expected_memory =[3,4,5,6,7,8,9,10] 
			    in assertWithMessage (readZSCIIString memory location == expected_memory) "Read ZCSII string from memory when terminator is missing"

test_splitMemoryCellToZChar = let cell = 6342  -- 6342 = 00110-00110-00110, or 6 6 6 
				  expected = [6,6,6]
			      in assertWithMessage (splitMemoryCellToZChar cell == expected)  "Split a single memory cell into 3 zchar"

test_splitMemoryCellToZChar_with_term = let cell = 39110  -- 39110 = 10110-00110-00110, or 6 6 6 with line terminator
				            expected = [6,6,6]
				        in assertWithMessage (splitMemoryCellToZChar cell == expected)  "Split a single memory cell into 3 zchar when last char has a line terminator"



					
test_convertZCharToASCIIChar_lower =   let	zchar = [6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31]
						register = LOWER
						expected = "abcdefghijklmnopqrstuvwxyz"
				      in assertWithMessage (Prelude.map (convertZCharToASCIIChar register) zchar == expected) "Convert zchar to ascii in lower case"

test_convertZCharToASCIIChar_upper =   let	zchar = [6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31]
						register = UPPER
						expected = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
				      in assertWithMessage (Prelude.map (convertZCharToASCIIChar register) zchar == expected) "Convert zchar to ascii in upper case"

test_convertZCharToASCIIChar_symbol = let	zchar = [6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31]
						register = SYMBOL
						expected = " 0123456789.,!?_#'\"/\\<-:()"
				      in assertWithMessage (Prelude.map (convertZCharToASCIIChar register) zchar == expected)  "Convert zchar to ascii in symbol case"


--------- TEST CASES ----------


assertWithMessage :: Bool -> String -> SpecWith ()
assertWithMessage condition message =  it message $ do condition

assert :: Bool -> SpecWith ()
assert condition = it "Get off your butt and write a message!" $ do condition


