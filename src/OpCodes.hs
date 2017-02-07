module OpCodes where

import           Control.Monad
import           Control.Monad.ST
import           Data.Bits
import           Data.Maybe
import           Data.STRef
import qualified Data.Vector.Unboxed as V
import           Data.Word           (Word16)
import           Debug.Trace
import           MemoryMap
import           System.Exit
import           ZSCIIString

data OpCode
  = QUIT
  | NEW_LINE
  | NOP
  | POP
  | INC Int
  | DEC Int
  | JUMP Int
  deriving (Show, Eq)


processOpCode
  :: OpCode -> MemoryMap -> MemoryMap
processOpCode x y =
  let result =(advanceProgramCounter (processOpCodeInternal x y))
  in trace ("calling process op code with state:" ++ show x ++ " and:" ++ show y ++ " with result:" ++ show result) (result)


processOpCodeInternal
  :: OpCode -> MemoryMap -> MemoryMap
processOpCodeInternal QUIT state = updateShouldTerminate state True
processOpCodeInternal NOP state = state
processOpCodeInternal NEW_LINE state = appendToStream1 state "\n"
processOpCodeInternal POP state = snd (popFromStack state)
processOpCodeInternal (JUMP offset) state =
  -- minus 1 because we already advanced one for this operation
  let newPC = ((programCounter state) + offset - 1)
  in updateProgramCounter state newPC

processOpCodeInternal (INC 0) state =
     let pop = popFromStack state
         val = (fromJust (fst pop)) + 1
         newState = snd pop
     in pushToStack newState val
processOpCodeInternal (INC var) state = setVar state (var-1) ((getVar state (var-1)) + 1)

processOpCodeInternal (DEC 0) state =
     let pop = popFromStack state
         val = (fromJust (fst pop)) - 1
         newState = snd pop
     in pushToStack newState val
processOpCodeInternal (DEC var) state = setVar state (var-1) ((getVar state (var-1)) - 1)




