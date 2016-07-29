module MemoryMap
where 

import Data.Word(Word16)
import Data.Bits
import Data.Maybe
import qualified Data.Vector.Unboxed as V
import Control.Monad.ST 
import Control.Monad
import Data.STRef



-- Requirments 1.x and 2.x: The memory map consists of a list of 2-byte Words.
-- The VM will decide how to interpret each word.  


type MemoryCell = Word16
type Memory = V.Vector  MemoryCell
type Location = Int
--
-- ZChar interpretation will depend upon the current shift register.
data ShiftRegister = 
   UPPER | UPPER_THEN_LOWER | UPPER_THEN_SYMBOL
 | LOWER | LOWER_THEN_UPPER | LOWER_THEN_SYMBOL
 | SYMBOL| SYMBOL_THEN_LOWER | SYMBOL_THEN_UPPER 
 deriving (Show, Eq)

-- The entire game state. This includes:
--	memory : All dynamic memory.
--	stack : the contents of the entire stack (this includes all stack
--		frames)
--	programCounter : The current program counter (location in dynamic memory
--		where execution is currently present.
--	stackFrames : A list of previous stacks. When a new routine is entered,
--		a new stack is created and the current stack should be pushed onto the
--		head of the "stackFrames". When a routine exits, the current stack
--		should be discarded and the current head of stackFrames should be popped
--		and return to its state.  
data MemoryMap = MemoryMap  {
	    memory :: Memory,
	    stack :: [MemoryCell],
	    programCounter :: Int,
	    stackFrames :: [[MemoryCell]],
	    shiftRegister :: ShiftRegister
	    } deriving (Show, Eq)

updateStack :: MemoryMap -> [MemoryCell] -> MemoryMap
updateStack current newStack = MemoryMap (memory current) newStack (programCounter current) (stackFrames current) (shiftRegister current)

updateMemoryMap :: MemoryMap -> Memory -> MemoryMap
updateMemoryMap current newMemory = MemoryMap newMemory (stack current) (programCounter current) (stackFrames current) (shiftRegister current)

updateShiftRegister :: MemoryMap -> ShiftRegister -> MemoryMap
updateShiftRegister current newShiftRegister = MemoryMap (memory current) (stack current) (programCounter current) (stackFrames current) newShiftRegister

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

updateStackHead :: MemoryMap -> (MemoryCell -> MemoryCell) -> MemoryMap
updateStackHead state function = let tupleStack = popFromStack state
				     firstTuple = fst tupleStack
				 in if firstTuple == Nothing then state
				    else let result = (function (fromJust firstTuple)) : (stack ( snd tupleStack) )
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




