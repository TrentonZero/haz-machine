module MemoryMap
where 

import Data.Word(Word16)
import Data.Bits
import qualified Data.Vector.Unboxed as V
import Control.Monad.ST 
import Control.Monad
import Data.STRef



-- Requirments 1.x and 2.x: The memory map consists of a list of 2-byte Words.
-- The VM will decide how to interpret each word.  


type MemoryCell = Word16
type Memory = V.Vector  MemoryCell
type Location = Int

data MemoryMap = MemoryMap  {
	    memory :: Memory,
	    stack :: [MemoryCell]
	    } deriving (Show, Eq)

updateStack :: MemoryMap -> [MemoryCell] -> MemoryMap
updateStack current newStack = MemoryMap (memory current) newStack

updateMemoryMap :: MemoryMap -> Memory -> MemoryMap
updateMemoryMap current newMemory = MemoryMap newMemory (stack current)


-- Write a single memory cell at a given location.
writeMemoryCell :: MemoryMap  -> Location -> MemoryCell -> MemoryMap
writeMemoryCell current loc newCell = 
			let result = (memory current) V.// [(loc,newCell)]
			in updateMemoryMap current result 



-- Read a single memory cell from a given location.
readMemoryCell :: MemoryMap -> Location -> Maybe MemoryCell
readMemoryCell current loc = (memory current) V.!? loc


-- This basically splits the memory into three, and replaces the middle with the
-- memory we intend to write. Probably not the most efficient way to do it,
-- especially given the cost of computing the length of the inbound  memory cell list.
writeMemory :: MemoryMap -> Location -> [MemoryCell] -> MemoryMap
writeMemory current loc cells = let zipped = zip [loc .. (loc+(length cells))] cells
				    result = (memory current) V.// zipped
				in updateMemoryMap current result 


pushToStack :: MemoryMap -> MemoryCell -> MemoryMap
pushToStack state cell = let result = cell : (stack state)
			 in updateStack state result 
		    
popFromStack :: MemoryMap -> (Maybe MemoryCell, MemoryMap)
popFromStack state = let result = popFromStackInt (stack state)
	             in (fst result, (updateStack state (snd result)))

peekFromStack :: MemoryMap -> (Maybe MemoryCell, MemoryMap)
peekFromStack state = let result = peekFromStackInt (stack state)
	             in (fst result, (updateStack state (snd result)))

popFromStackInt :: [MemoryCell] -> (Maybe MemoryCell, [MemoryCell])
popFromStackInt (x:stack)  = (Just x, stack)
popFromStackInt _ = (Nothing, [])

peekFromStackInt :: [MemoryCell] -> (Maybe MemoryCell, [MemoryCell])
peekFromStackInt (x:stack)  = (Just x, x : stack)
peekFromStackInt _ = (Nothing, [])

-------  LOCAL FUNCTIONS TO HELP OUT ----------- 

fst3 :: (a, b, c) -> a
fst3 (a,_,_) = a

snd3 :: (a, b, c) -> b
snd3 (_,b,_) = b

thrd3 :: (a, b, c) -> c
thrd3 (_,_,c) = c


-- Same as split at, but instead splits the list into three.
splitAt3 :: Int -> Int -> [a]  -> ([a], [a], [a])
splitAt3 loc1 loc2 cells =  let mytail = snd fullTail
				middle = fst fullTail
			    	myhead = fst fullHead
				fullHead = (splitAt loc1 cells)
				fullTail = (splitAt (loc2-loc1) (snd fullHead))
				in (myhead , middle , mytail)




---- EXPERIMENTATION RELATED TO MUTABLE VECTORS AND ST MONAD, ETC. 

--createMutableVector :: Location -> ST s (MutMemoryMap s a)
-- createMutableVector size = M.new size 

--readMutableVector :: ST s (MutMemoryMap s a) -> Memory
--readMutableVector x = M.tail (runST x)
		      

--mlength :: MutMemoryMap  s a -> ST s Int
-- length filt = (succ . snd) `liftM` M.getBounds (memory filt)
--mlength filt = M.length memory
--
--newSTRef :: a -> ST s (STRef s a)
--newArray_ :: Ix i => (i, i) -> ST s (STArray s i e)


--oneST :: Memory -> ( [MemoryCell] -> [MemoryCell] ) -> ST s Memory -- note that this works correctly for any s
--oneST oldstate f = do var <- newSTRef oldstate
                      --modifySTRef var f
                      --modifySTRef var f
	              --readSTRef  var
		       
--oneSTRef :: ST s Memory -- note that this works correctly for any s
--oneSTRef =  do var <- newSTRef [1,2]
	       --writeSTRef var [1,2,3]
	       --readSTRef var

--twoST ::  Memory  -> ST s1 Memory -- note that this works correctly for any s
--twoST oldstate = do var <-  newSTRef oldstate
                    --modifySTRef var (tail)
	            --readSTRef var


--tailCells :: [MemoryCell] -> [MemoryCell]
--tailCells x = tail x

--one :: Memory
--one = runST (oneST [1,2,3,4,5,6] tailCells)
--two :: Memory
--two = runST (twoST one)


--writeMemoryCellM :: ST s (MutMemoryMap s a) -> Location -> MemoryCell -> Maybe Bool
-- writeMemoryCellM current loc newCell = do M.write current loc newCell
	
-- readMemoryCellM :: M.MVector MemoryCell -> Location -> MemoryCell
-- readMemoryCellM current loc = do M.read current loc




