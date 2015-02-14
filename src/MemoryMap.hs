module MemoryMap
(
    MemoryMap(memory),
    MemoryMap.Memory, 
    MemoryMap.MemoryCell, 
    MemoryMap.Location, 
    MemoryMap.tests, 
    MemoryMap.writeMemoryCell,
    MemoryMap.writeMemory,
    MemoryMap.readMemoryCell
) 
where 

import Data.Word(Word16)
import Data.Bits
import Distribution.TestSuite


-- Requirments 1.x and 2.x: The memory map consists of a list of 2-byte Words.
-- The VM will decide how to interpret each word.  


type MemoryCell = Word16
type Memory = [MemoryCell] 
type Location = Int

data MemoryMap = MemoryMap  {
	    memory :: Memory
	    } deriving (Show)

-- Write a single memory cell at a given location.
writeMemoryCell :: Memory -> Location -> MemoryCell -> Memory
writeMemoryCell current loc newCell = 
			let (x,_:xs) = (splitAt loc current)
			in x ++ newCell : xs					    
test_writeMemoryCell_base = let memory = [1,2,3,4,5,6,7,8,9,10]
				location = 2
				cell = 0
				expected = [1,2,0,4,5,6,7,8,9,10]
			    in assert (writeMemoryCell memory location cell == expected)

-- Read a single memory cell from a given location.
readMemoryCell :: Memory -> Location -> Maybe MemoryCell
readMemoryCell current loc = let splitLoc = (snd (splitAt loc current) )
				in if not (null splitLoc) then 
				  Just (head splitLoc)
				else 
				  Nothing

test_readMemoryCell_base =  let memory = [1,2,3,4,5,6,7,8,9,10]
				location = 2
				expected = Just 3
			    in assert (readMemoryCell memory location == expected)
test_readMemoryCell_oob =  let  memory = [1,2,3,4,5,6,7,8,9,10]
				location = 11
				expected = Nothing 
			    in assert (readMemoryCell memory location == expected)

-- This basically splits the memory into three, and replaces the middle with the
-- memory we intend to write. Probably not the most efficient way to do it,
-- especially given the cost of computing the length of the inbound  memory cell list.
writeMemory :: Memory -> Location -> [MemoryCell] -> Memory
writeMemory current loc cells = let loc1 = loc
				    loc2 = loc + (length cells)
				    tuple = splitAt3 loc1 loc2 current
				    top = fst3 tuple
				    middle = cells
				    bottom = thrd3 tuple
				    in top ++ middle ++  bottom
test_writeMemory_base = let memory = [1,2,3,4,5,6,7,8,9,10]
			    location = 2
			    cells = [0,1,2,3]
			    expected = [1,2,0,1,2,3,7,8,9,10]
			in assert (writeMemory memory location cells == expected)

	

		    
-------  LOCAL FUNCTIONS TO HELP OUT ----------- 

fst3 :: (a, b, c) -> a
fst3 (a,_,_) = a
test_fst3_base = assert (fst3 (1,2,3) == 1)

snd3 :: (a, b, c) -> b
snd3 (_,b,_) = b
test_snd3_base = assert (snd3 (1,2,3) == 2) 

thrd3 :: (a, b, c) -> c
thrd3 (_,_,c) = c
test_thrd3_base = assert (thrd3 (1,2,3) == 3) 


-- Same as split at, but instead splits the list into three.
splitAt3 :: Int -> Int -> [a]  -> ([a], [a], [a])
splitAt3 loc1 loc2 cells =  let mytail = snd fullTail
				middle = fst fullTail
			    	myhead = fst fullHead
				fullHead = (splitAt loc1 cells)
				fullTail = (splitAt (loc2-loc1) (snd fullHead))
				in (myhead , middle , mytail)
test_splitAt3_base = assertWithMessage ((splitAt3 1 3 [0,1,2,3,4,5,6,7,8,9]) == ([0],[1,2],[3,4,5,6,7,8,9])) "incorrectly split"



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
		    Test t_fst3_base,
		    Test t_snd3_base,
		    Test t_thrd3_base,
		    Test t_splitAt3_base,
		    Test t_writeMemoryCell_base,
		    Test t_writeMemory_base ,
		    Test t_readMemoryCell_base,
		    Test t_readMemoryCell_oob
		]
    where
	t_fst3_base = TestInstance
	    { run = return $ test_fst3_base
		    , name = "test_fst3_base"
		    , tags = []
		    , options = []
		    , setOption = \_ _ -> Right t_fst3_base
	    }
	t_snd3_base = TestInstance
	    { run = return $ test_snd3_base
		    , name = "test_snd3_base"
		    , tags = []
		    , options = []
		    , setOption = \_ _ -> Right t_snd3_base
	    }
	t_thrd3_base = TestInstance
	    { run = return $ test_thrd3_base
		    , name = "test_thrd3_base"
		    , tags = []
		    , options = []
		    , setOption = \_ _ -> Right t_thrd3_base
	    }
	t_splitAt3_base = TestInstance
	    { run = return $ test_splitAt3_base
		    , name = "test_splitAt3_base"
		    , tags = []
		    , options = []
		    , setOption = \_ _ -> Right t_splitAt3_base
	    }
	t_writeMemoryCell_base = TestInstance
	    { run = return $ test_writeMemoryCell_base
		    , name = "test_writeMemoryCell_base"
		    , tags = []
		    , options = []
		    , setOption = \_ _ -> Right t_writeMemoryCell_base
	    }
	t_writeMemory_base = TestInstance
	    { run = return $ test_writeMemory_base
		    , name = "test_writeMemory_base"
		    , tags = []
		    , options = []
		    , setOption = \_ _ -> Right t_writeMemory_base
	    }
	t_readMemoryCell_base = TestInstance
	    { run = return $ test_readMemoryCell_base
		    , name = "test_readMemoryCell_base"
		    , tags = []
		    , options = []
		    , setOption = \_ _ -> Right t_readMemoryCell_base
	    }
	t_readMemoryCell_oob = TestInstance
	    { run = return $ test_readMemoryCell_oob
		    , name = "test_readMemoryCell_oob"
		    , tags = []
		    , options = []
		    , setOption = \_ _ -> Right t_readMemoryCell_oob
	    }








