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


{-|
All Op Codes

Zero Operand


Zero-operand opcodes 0OP
  rtrue
  rfalse
  print (literal-string)	print
  print_ret
  nop
  save ?(label)
  save -> (result)
  restore ?(label)
  restore -> (result)
  restart
  ret_popped
  pop
  catch
  quit
  new_line
  show_status
  verify
  extended
  piracy


-}

data OpCode
  = QUIT
  | NEW_LINE
  | NOP
  | POP
  | INC Int
  | DEC Int
  | JUMP Int
  | JZ Int Int
  | JL Int Int Int
  | JG Int Int Int
  | PRINT_ADDR Int
  | PRINT [ZChar]
  deriving (Show, Eq)


{-|
We can fold this with the either of the following:

right to left:   foldr processOpCode defaultMemoryMap [NOP, NOP, NOP, NEW_LINE, INC 4, JUMP 4]
left to right:

 import Data.Foldable
reorder a b = processOpCode b a
 foldl' reorder defaultMemoryMap [NOP, NOP, NOP, JUMP 4]

it must be foldl' to eliminate pointless traversals of the list

-}
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
processOpCodeInternal (JUMP offset) state = performJump state offset

processOpCodeInternal (JZ 0 offset) state = performJump state offset
processOpCodeInternal (JZ operand offset) state = state


processOpCodeInternal (JL operand_a operand_b offset) state
  | operand_a >= operand_b = state
processOpCodeInternal (JL operand_a operand_b offset) state = performJump state offset

processOpCodeInternal (JG operand_a operand_b offset) state
  | operand_a <= operand_b = state
processOpCodeInternal (JG operand_a operand_b offset) state = performJump state offset

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

processOpCodeInternal (PRINT_ADDR addr) state = appendToStream1 state $ readASCIIString state addr


processOpCodeInternal (PRINT zstring) state = appendToStream1 state $ catMaybes $ evaluateZString state zstring



performJump state offset =
    -- minus 1 because we already advanced one for this operation
  let newPC = ((programCounter state) + offset - 1)
  in updateProgramCounter state newPC
