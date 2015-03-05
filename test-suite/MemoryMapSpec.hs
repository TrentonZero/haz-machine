-- test-suite/MemoryMapSpec.hs
module MemoryMapSpec (spec) where
import MemoryMap 
import Test.Hspec
import Test.Hspec.QuickCheck


spec :: Spec
spec = parallel $ do 
    describe "MemoryMap Tests" $ do
	test_writeMemoryCell_base
	test_readMemoryCell_base
	test_readMemoryCell_oob
	test_writeMemory_base
	test_fst3_base
	test_snd3_base
	test_thrd3_base
	test_splitAt3_base


--- old stuff

test_writeMemoryCell_base :: SpecWith ()
test_writeMemoryCell_base = let memory = [1,2,3,4,5,6,7,8,9,10]
				location = 2
				cell = 0
				expected = [1,2,0,4,5,6,7,8,9,10]
			    in assertWithMessage (writeMemoryCell memory location cell == expected) "Write one memorycell"


test_readMemoryCell_base =  let memory = [1,2,3,4,5,6,7,8,9,10]
				location = 2
				expected = Just 3
			    in assertWithMessage (readMemoryCell memory location == expected) "Read memory"
test_readMemoryCell_oob =  let  memory = [1,2,3,4,5,6,7,8,9,10]
				location = 11
				expected = Nothing 
			    in assertWithMessage (readMemoryCell memory location == expected) "Read memory out of bounds."



			    
test_writeMemory_base = let memory = [1,2,3,4,5,6,7,8,9,10]
			    location = 2
			    cells = [0,1,2,3]
			    expected = [1,2,0,1,2,3,7,8,9,10]
			in assertWithMessage (writeMemory memory location cells == expected) "Write Memory range"


test_fst3_base = assertWithMessage (fst3 (1,2,3) == 1) "First in a triple"

test_snd3_base = assertWithMessage (snd3 (1,2,3) == 2)  "Second in a triple"

test_thrd3_base = assertWithMessage (thrd3 (1,2,3) == 3)  "Third in a triple"

test_splitAt3_base = assertWithMessage ((splitAt3 1 3 [0,1,2,3,4,5,6,7,8,9]) == ([0],[1,2],[3,4,5,6,7,8,9])) "Split into triplets"




--------- TEST CASES ----------

--assertWithMessage :: Bool ->  String -> Progress
--assertWithMessage condition fail_message =
		 --if condition then 
		    --Finished Pass
		--else
		    --Finished $ Fail fail_message

--assert :: Bool -> Progress
--assert condition = assertWithMessage condition "no match"

assertWithMessage :: Bool -> String -> SpecWith ()
assertWithMessage condition message =  it message $ do condition

assert :: Bool -> SpecWith ()
assert condition = it "Get off your butt and write a message" $ do condition

