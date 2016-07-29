-- test-suite/MemoryMapSpec.hs
module MemoryMapSpec (spec) where
import MemoryMap
import Test.Hspec
import Test.Hspec.QuickCheck
import Data.Vector.Unboxed


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
	test_popFromSmallStack
	test_popFromPopulatedStack
	test_pushToPopulatedStack
	test_pushToStack
	test_peekFromPopulatedStack 
	test_peekFromSmallStack 
	test_peekFromEmptyStack 
	test_updatePopulatedStack 
	test_updateSmallStack 
	test_updateEmptyStack 

--- old stuff

test_writeMemoryCell_base :: SpecWith ()
test_writeMemoryCell_base = let memory = MemoryMap (fromList [1,2,3,4,5,6,7,8,9,10]) [] 0 [] LOWER
				location = 2
				cell = 0
				expected = MemoryMap (fromList [1,2,0,4,5,6,7,8,9,10]) [] 0 [] LOWER
			    in assertWithMessage (writeMemoryCell memory location cell == expected) "Write one memorycell"


test_readMemoryCell_base =  let memory = MemoryMap (fromList [1,2,3,4,5,6,7,8,9,10]) [] 0 [] LOWER
				location = 2
				expected = Just 3
			    in assertWithMessage (readMemoryCell memory location == expected) "Read memory"
test_readMemoryCell_oob =  let  memory = MemoryMap (fromList [1,2,3,4,5,6,7,8,9,10]) [] 0 [] LOWER
				location = 11
				expected = Nothing 
			    in assertWithMessage (readMemoryCell memory location == expected) "Read memory out of bounds."



			    
test_writeMemory_base = let memory = MemoryMap (fromList [1,2,3,4,5,6,7,8,9,10]) [] 0 [] LOWER
			    location = 2
			    cells = [0,1,2,3]
			    expected = MemoryMap (fromList [1,2,0,1,2,3,7,8,9,10]) [] 0 [] LOWER
			in assertWithMessage (writeMemory memory location cells == expected) "Write Memory range"



test_pushToStack = let  stack = MemoryMap (fromList [100]) [] 0 [] LOWER
			cell = 1000
			expected = MemoryMap (fromList [100]) [1000] 0 [] LOWER
		    in  assertWithMessage (pushToStack stack cell == expected) "Pushing onto empty stack"

test_pushToPopulatedStack = let  stack = MemoryMap (fromList []) [500] 0 [] LOWER
				 cell = 1000
				 expected = MemoryMap (fromList []) [1000, 500] 0 [] LOWER
			    in  assertWithMessage (pushToStack stack cell == expected) "Pushing onto populated stack"


test_popFromPopulatedStack = let  stack = MemoryMap (fromList []) [1000, 500, 122] 0 [] LOWER
			          expected = (Just 1000, MemoryMap (fromList []) [500,122] 0 [] LOWER) 
			     in  assertWithMessage (popFromStack stack == expected) "Popping from populated stack"

test_popFromSmallStack = let      stack = MemoryMap (fromList [])[1000] 0 [] LOWER
			          expected = (Just 1000, MemoryMap (fromList []) [] 0 [] LOWER)
			     in  assertWithMessage (popFromStack stack == expected) "Popping from populated stack"
			     
test_popFromEmptyStack = let      stack = MemoryMap (fromList [100]) [] 0 [] LOWER
			          expected = (Nothing, MemoryMap (fromList [100]) [] 0 [] LOWER)
			     in  assertWithMessage (popFromStack stack == expected) "Popping from empty stack"

test_peekFromPopulatedStack = let  stack = MemoryMap (fromList []) [1000, 500, 122] 0 [] LOWER
			           expected = (Just 1000,MemoryMap (fromList []) [1000,500,122] 0 [] LOWER)
			     in  assertWithMessage (peekFromStack stack == expected) "peeking from peekulated stack"

test_peekFromSmallStack = let      stack = MemoryMap (fromList []) [1000] 0 [] LOWER
			           expected = (Just 1000, MemoryMap (fromList []) [1000] 0 [] LOWER)
			     in  assertWithMessage (peekFromStack stack == expected) "peeking from peekulated stack"
			     
test_peekFromEmptyStack = let      stack = MemoryMap (fromList [100]) [] 0 [] LOWER
			           expected = (Nothing,MemoryMap (fromList [100]) [] 0 [] LOWER)
			     in  assertWithMessage (peekFromStack stack == expected) "peeking from empty stack"

test_updatePopulatedStack = let  stack = MemoryMap (fromList []) [1000, 500, 122] 0 [] LOWER
			         expected = MemoryMap (fromList []) [1001,500,122] 0 [] LOWER
			     in  assertWithMessage (updateStackHead stack (+1) == expected) "updating populated stack"

test_updateSmallStack = let      stack = MemoryMap (fromList []) [1000] 0 [] LOWER
			         expected = (MemoryMap (fromList []) [1001] 0 [] LOWER)
			     in  assertWithMessage (updateStackHead  stack (+1)== expected) "updating small stack"
			     
test_updateEmptyStack = let      stack = MemoryMap (fromList [100]) [] 0 [] LOWER
			         expected = MemoryMap (fromList [100]) [] 0 [] LOWER
			     in  assertWithMessage (updateStackHead stack (+1)== expected) "updating empty stack"

test_fst3_base = assertWithMessage (fst3 (1,2,3) == 1) "First in a triple"

test_snd3_base = assertWithMessage (snd3 (1,2,3) == 2)  "Second in a triple"

test_thrd3_base = assertWithMessage (thrd3 (1,2,3) == 3)  "Third in a triple"

test_splitAt3_base = assertWithMessage ((splitAt3 1 3 [0,1,2,3,4,5,6,7,8,9]) == ([0],[1,2],[3,4,5,6,7,8,9])) "Split into triplets"




--------- TEST CASES ----------


assertWithMessage :: Bool -> String -> SpecWith ()
assertWithMessage condition message =  it message $ do condition

assert :: Bool -> SpecWith ()
assert condition = it "Get off your butt and write a message" $ do condition

