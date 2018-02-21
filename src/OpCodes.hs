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
  | PIRACY Int
  | INC Int
  | DEC Int
  | DEC_CHK Int Int Int
  | JUMP Int
  | JZ Int Int
  | JL Int Int Int
  | JG Int Int Int
  | JE Int Int Int
  | PRINT_ADDR Int
  | PRINT [ZChar]
  | ADD Int Int Int
  | SUB Int Int Int
  | MUL Int Int Int
  | DIV Int Int Int
  deriving (Show, Eq)



process
  :: MemoryMap -> MemoryMap
process memory =
  processOpCode (getOpCode memory) memory


{-|
How the HELL do I get an opcode?

Answer: It's complicated, and it is on page 26-31 of the Z Machine Standards document. Below is a summary:

All instructions have a form: (long, short, extended, variable).
Extended is V5+ only.

PreV5:
- First 1-2 Bytes: OpCode (2 byte optional)
    - Bits 1-2 determine the form
        - 11 : Variable form
            - Bit 5 = 0 : 2 operand
            - Bit 5 = 1 : variable operands
            - Bottom 5 bits: op code number
                - In V5+, if 190 (0xBE), always variable operands, and op code number is in byte 2.
        - 10 : Short form
            - Bits 4-5 describe operand count
                - 11 : 0 OP
                - else 1 OP
                - short form is never 2 OP or  var OP
            - Bottom 4 bits give the opcode number  (This is ambiguous and I think there is a typo in the doc)
        - ELSE: Long form
            - Always 2OP
            - Bottom 5 bits are opcode number
- Next byte (>v5): Opcode number for Extended form (V5+ only), otherwise...
- Next 0-2 Bytes (<v5): (depending on form and number of operands)
    - Short Form Opcode
        - Bits 4-5 = Type
    - Long form Opcode
        - Bit 6 = Type of Operand 1
            - 0 = small constant
            - 1 = variable
            - Large Constant impossible, requires variable form
        - Bit 5 = Type of Operand 2
            - same as above
    - Variable form Opcode
        - A single byte containing 4 2-bit operand types
        - Bits 6-7: First operand type
            - 00 = Large constant (2 bytes)
            - 01 = Small constant (1 byte)
            - 10 = Variable (1 byte)
            - 11 = Omitted
                - Once a type is omitted, no further operands should be read
        - Bits 4-5: Second operand type
        - Bits 2-3: Third operand type
        - Bits 0-1: Fourth operand type
- Up to 16 bites of operands, as determined by opcode form and type. There may be 0 of these.
- Optional Store variable depending on op code (1 byte)
- Optional Branch offset  depending on op code (1 or 2 bytes)
- Optional Text to print depending on op code (unlimited length until string terminated)

So... time to encode all that, but on the upside, it is finally really coming together how this could be just a foldl'
on the memory map with process function to actually execute the VM.

Also, even that summary leaves out a lot, go to section 4 of the Z Machine standard (pg26) when you actually work.


-}

getOpCode
   :: MemoryMap -> OpCode
getOpCode _ = QUIT
--   let op_code_cell = readMemoryCell (programCounter memory) memory



{-|
Note, this comment is out of date, but kept around because it's a useful idea.

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
  let result = advanceProgramCounter $ processOpCodeInternal x y
  in trace ("calling process op code with state:" ++ show x ++ " and:" ++ show y ++ " with result:" ++ show result) result


processOpCodeInternal
  :: OpCode -> MemoryMap -> MemoryMap
processOpCodeInternal QUIT state = updateShouldTerminate state True
processOpCodeInternal NOP state = state
processOpCodeInternal NEW_LINE state = appendToStream1 state "\n"
processOpCodeInternal POP state = snd (popFromStack state)

processOpCodeInternal (ADD operand_a operand_b var) state = setVar state (var-1) (operand_a + operand_b)

processOpCodeInternal (SUB operand_a operand_b var) state = setVar state (var-1) (operand_a - operand_b)

processOpCodeInternal (MUL operand_a operand_b var) state = setVar state (var-1) (operand_a * operand_b)

processOpCodeInternal (DIV operand_a operand_b var) state = setVar state (var-1) (operand_a `div` operand_b)

processOpCodeInternal (JUMP offset) state = performJump state offset

processOpCodeInternal (JZ 0 offset) state = performJump state offset
processOpCodeInternal (JZ operand offset) state = state


processOpCodeInternal (JL operand_a operand_b offset) state
  | operand_a >= operand_b = state
processOpCodeInternal (JL operand_a operand_b offset) state = performJump state offset

processOpCodeInternal (JG operand_a operand_b offset) state
  | operand_a <= operand_b = state
processOpCodeInternal (JG operand_a operand_b offset) state = performJump state offset

processOpCodeInternal (JE operand_a operand_b offset) state
  | operand_a /= operand_b = state
processOpCodeInternal (JE operand_a operand_b offset) state = performJump state offset

processOpCodeInternal (INC 0) state =
     let pop = popFromStack state
         val = fromJust (fst pop) + 1
         newState = snd pop
     in pushToStack newState val
processOpCodeInternal (INC var) state = setVar state (var-1) (getVar state (var-1) + 1)

processOpCodeInternal (DEC 0) state =
     let pop = popFromStack state
         val = fromJust (fst pop) - 1
         newState = snd pop
     in pushToStack newState val
processOpCodeInternal (DEC var) state = setVar state (var-1) (getVar state (var-1) - 1)

processOpCodeInternal (DEC_CHK var val label) state =
  let stateAfterDec = processOpCodeInternal (DEC var) state
      newVarVal     = getVar stateAfterDec (var - 1)
      stateAfterJmp = processOpCodeInternal (JL newVarVal val label) stateAfterDec
   in stateAfterJmp

processOpCodeInternal (PRINT_ADDR addr) state = appendToStream1 state $ readASCIIString state addr


processOpCodeInternal (PRINT zstring) state = appendToStream1 state $ catMaybes $ evaluateZString state zstring


processOpCodeInternal (PIRACY loc) state = performJump state loc

performJump state offset =
    -- minus 1 because we already advanced one for this operation
  let newPC = (programCounter state + offset - 1)
  in updateProgramCounter state newPC
