-- test-suite/OpCodesSpec.hs
module OpCodesSpec
  (spec)
  where

import           Data.Maybe
import qualified Data.Vector.Unboxed   as V
import           Debug.Trace
import           MemoryMap
import           OpCodes
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           ZSCIIString

spec :: Spec
spec =
  parallel $
  describe "OpCodes Tests" $
  do test_QUIT
     test_pop
     test_nop
     test_newLine
     test_INC
     test_INC_STACK
     test_DEC
     test_DEC_STACK
     test_DEC_CHK
     test_JUMP
     test_JUMP_negative
     test_JUMP_zero
     test_PIRACY
     test_JZ_zero
     test_JZ_notzero
     test_JL_lessthan
     test_JL_equal
     test_JL_greater
     test_JG_lessthan
     test_JG_equal
     test_JG_greater
     test_JE_lessthan
     test_JE_equal
     test_JE_greater
     test_PRINT_ADDR_aaa
     test_PRINT_ADDR_respect_term
     test_PRINT_aaa
     test_ADD
     test_ADD_2
     test_SUB
     test_MUL
     test_DIV
     test_DIV_notwhole
     test_getOperandTypes_VARIABLE_FORM
     test_getOperandTypes_VARIABLE_FORM_2

test_nop =
  let memory = defaultMemoryMap
      expected = memory { programCounter = 1}
      result = processOpCode NOP memory
  in assertWithMessage result expected "Should advance the program counter and nothing else"


test_pop =
  let memory = defaultMemoryMap { stack = [1,2,3] }
      expected = memory { stack = [2,3], programCounter = 1}
      result = processOpCode POP memory
  in assertWithMessage result expected "Should pop top value off the stack"

test_newLine =
  let memory = defaultMemoryMap
      expected = memory { stream1 = "\n", programCounter = 1}
      result = processOpCode NEW_LINE memory
  in assertWithMessage result expected "new line should advance the program counter"


test_QUIT =
  let memory = defaultMemoryMap { memory = V.fromList [1,2,3,4,5,6,7,0xFFFF,8,9,10] }
      expected = memory { shouldTerminate = True, programCounter = 1 }
      result = processOpCode QUIT memory
  in assertWithMessage result expected "Should set the terminate flag so that MAIN IO Can kill it"

test_INC =
  let memory = defaultMemoryMap { vars = [0] }
      expected = memory { vars = [1], programCounter = 1 }
      result = processOpCode (INC 1) memory
  in assertWithMessage result expected "Should increment variable. "

test_INC_STACK =
  let memory = defaultMemoryMap { vars = [0], stack = [1,2,3] }
      expected = memory { vars = [0], stack = [2,2,3], programCounter = 1 }
      result = processOpCode (INC 0) memory
  in assertWithMessage result expected "Should increment head of stack. "

test_DEC =
  let memory = defaultMemoryMap { vars = [100] }
      expected = memory { vars = [99], programCounter = 1 }
      result = processOpCode (DEC 1) memory
  in assertWithMessage result expected "Should decrement variable."

test_DEC_CHK =
  let memory = defaultMemoryMap { memory = V.fromList [0,0,0,0,0,0], vars = [100] }
      expected = memory { vars = [99], programCounter = 5 }
      result = processOpCode (DEC_CHK 1 100 5) memory
  in assertWithMessage result expected "Should decrement variable and jump."

test_DEC_STACK =
  let memory = defaultMemoryMap { vars = [0], stack = [3,2,1] }
      expected = memory { vars = [0], stack = [2,2,1], programCounter = 1 }
      result = processOpCode (DEC 0) memory
  in assertWithMessage result expected "Should decrement head of stack."

test_JUMP =
  let memory = defaultMemoryMap { programCounter = 132}
      expected = memory { programCounter = 232 }
      result = processOpCode (JUMP 100) memory
  in assertWithMessage result expected "Should jump program counter by 100."

test_JUMP_negative =
  let memory = defaultMemoryMap { programCounter = 132}
      expected = memory { programCounter = 32 }
      result = processOpCode (JUMP (-100)) memory
  in assertWithMessage result expected "Should jump program counter by -100."

test_JUMP_zero =
  let memory = defaultMemoryMap { programCounter = 132}
      expected = memory
      result = processOpCode (JUMP 0) memory
  in assertWithMessage result expected "Should leave program counter unchanged."

test_PIRACY =
  let memory = defaultMemoryMap { programCounter = 132}
      expected = memory { programCounter = 142 }
      result = processOpCode (PIRACY 10) memory
  in assertWithMessage result expected "Interpreter should not treat games as pirated"

test_JZ_zero =
  let memory = defaultMemoryMap { programCounter = 132}
      expected = memory { programCounter = 142 }
      result = processOpCode (JZ 0 10) memory
  in assertWithMessage result expected "should jump since passed zero"

test_JZ_notzero =
  let memory = defaultMemoryMap { programCounter = 132}
      expected = memory { programCounter = 133 }
      result = processOpCode (JZ 10 10) memory
  in assertWithMessage result expected "should not jump since passed ten"

test_JL_lessthan =
  let memory = defaultMemoryMap { programCounter = 132}
      expected = memory { programCounter = 142 }
      result = processOpCode (JL 12 15 10) memory
  in assertWithMessage result expected "should jump since a less than b"

test_JL_equal =
  let memory = defaultMemoryMap { programCounter = 132}
      expected = memory { programCounter = 133 }
      result = processOpCode (JL 15 15 10) memory
  in assertWithMessage result expected "should not jump since a equals b"

test_JL_greater =
  let memory = defaultMemoryMap { programCounter = 132}
      expected = memory { programCounter = 133 }
      result = processOpCode (JL 20 15 10) memory
  in assertWithMessage result expected "should not jump since a greater than b"

test_JG_lessthan =
  let memory = defaultMemoryMap { programCounter = 132}
      expected = memory { programCounter = 133 }
      result = processOpCode (JG 12 15 10) memory
  in assertWithMessage result expected "should not jump since a less than b"

test_JG_equal =
  let memory = defaultMemoryMap { programCounter = 132}
      expected = memory { programCounter = 133 }
      result = processOpCode (JG 15 15 10) memory
  in assertWithMessage result expected "should not jump since a equals b"

test_JG_greater =
  let memory = defaultMemoryMap { programCounter = 132}
      expected = memory { programCounter = 142 }
      result = processOpCode (JG 20 15 10) memory
  in assertWithMessage result expected "should jump since a greater than b"


test_JE_lessthan =
  let memory = defaultMemoryMap { programCounter = 132}
      expected = memory { programCounter = 133 }
      result = processOpCode (JE 12 15 10) memory
  in assertWithMessage result expected "should not jump since a less than b"

test_JE_equal =
  let memory = defaultMemoryMap { programCounter = 132}
      expected = memory { programCounter = 142 }
      result = processOpCode (JE 15 15 10) memory
  in assertWithMessage result expected "should jump since a equals b"

test_JE_greater =
  let memory = defaultMemoryMap { programCounter = 132}
      expected = memory { programCounter = 133 }
      result = processOpCode (JE 20 15 10) memory
  in assertWithMessage result expected "should not jump since a greater than b"



test_PRINT_ADDR_aaa =
  let memory = defaultMemoryMap { memory = V.fromList [6342, 39110], shiftRegister = LOWER}
      expected = memory { stream1 = "aaaaaa", programCounter = 1}
      result = processOpCode (PRINT_ADDR 0) memory
  in assertWithMessage result expected "should append 'aaaaaa' to stream1"

test_PRINT_ADDR_respect_term =
  let memory = defaultMemoryMap { memory = V.fromList [6342, 39110, 6342], shiftRegister = LOWER}
      expected = memory { stream1 = "aaaaaa", programCounter = 1}
      result = processOpCode (PRINT_ADDR 0) memory
  in assertWithMessage result expected "should append 'aaaaaa' to stream1, respecting the string terminator"

test_PRINT_aaa =
  let memory = defaultMemoryMap { memory = V.fromList [6342, 39110], shiftRegister = LOWER}
      expected = memory { stream1 = "aaaaaa", programCounter = 1}
      result = processOpCode (PRINT [6,6,6,6,6,6]) memory
  in assertWithMessage result expected "should append 'aaaaaa' to stream1"

test_ADD =
  let memory = defaultMemoryMap { vars = [0] }
      expected = memory { vars = [10], programCounter = 1}
      result = processOpCode (ADD 5 5 1) memory
  in assertWithMessage result expected "should add 5 and 5 and store result in var 1"

test_ADD_2 =
  let memory = defaultMemoryMap { vars = [0,0] }
      expected = memory { vars = [0,100], programCounter = 1}
      result = processOpCode (ADD 50 50 2) memory
  in assertWithMessage result expected "should add 50 and 50 and store result in var 2"

test_SUB =
  let memory = defaultMemoryMap { vars = [0] }
      expected = memory { vars = [10], programCounter = 1}
      result = processOpCode (SUB 15 5 1) memory
  in assertWithMessage result expected "should subtract 15 and 5 and store result in var 1"

test_MUL =
  let memory = defaultMemoryMap { vars = [0] }
      expected = memory { vars = [50], programCounter = 1}
      result = processOpCode (MUL 10 5 1) memory
  in assertWithMessage result expected "should multiply 10 and 5 and store result in var 1"


test_DIV =
  let memory = defaultMemoryMap { vars = [0] }
      expected = memory { vars = [2], programCounter = 1}
      result = processOpCode (DIV 10 5 1) memory
  in assertWithMessage result expected "should divide 10 by 5 and store result in var 1"


test_DIV_notwhole =
  let memory = defaultMemoryMap { vars = [0] }
      expected = memory { vars = [1], programCounter = 1}
      result = processOpCode (DIV 10 7 1) memory
  in assertWithMessage result expected "should divide 10 by 7 and get 1 and store result in var 1"

test_getOperandTypes_VARIABLE_FORM =
  let cell = 0xFF
      expected = [OMITTED]
      result = getOperandTypes VARIABLE_FORM cell
  in assertWithMessage result expected "Should neglect everything after first omitted"

test_getOperandTypes_VARIABLE_FORM_2 =
  let cell = 0xF0
      expected = [LARGE, LARGE, OMITTED]
      result = getOperandTypes VARIABLE_FORM cell
  in assertWithMessage result expected "Should neglect everything after LARGE,LARGE,OMIT"

test_getOperandTypes_VARIABLE_FORM_3 =
  let cell = 0x00
      expected = [LARGE, LARGE, LARGE, LARGE]
      result = getOperandTypes VARIABLE_FORM cell
  in assertWithMessage result expected "Should show all larges"

--------- TEST CASES ----------
assertWithMessage result expected message =
  let messageL = message ++ "\n\tresult: " ++ show result ++ "\n\texpected: " ++ show expected
  in it messageL (result == expected)

assert :: Bool -> SpecWith ()
assert = it "Get off your butt and write a message!"

