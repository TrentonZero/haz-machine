-- test-suite/OpCodesSpec.hs
module OpCodesSpec
  (spec)
  where

import           Data.Bits
import           Data.Maybe
import qualified Data.Vector.Unboxed   as V
import           Data.Word             (Word16)
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
     --test_PIRACY
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
     test_PRINT_ADDR_respect_term
     test_PRINT_aaa
     test_PRINT_hello
     test_ADD
     test_ADD_2
     test_SUB
     test_MUL
     test_DIV
     test_DIV_notwhole
     test_getOperandTypes_VARIABLE_FORM
     test_getOperandTypes_SHORT_FORM
     test_getOperandType_SHORT_FORM
     test_getOpCode_0OP
     --test_getOperand
     --test_getOperands

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
  let memory = defaultMemoryMap { memory = V.fromList [1,2,3,4,5,6,7,0xFF, 0xFF,8,9,10] }
      expected = memory { shouldTerminate = True, programCounter = 1 }
      result = processOpCode QUIT memory
  in assertWithMessage result expected "Should set the terminate flag so that MAIN IO Can kill it"

test_INC =
  let memory = defaultMemoryMap { vars = [0] }
      expected = memory { vars = [1], programCounter = 1 }
      result = processOpCode (INC (STORE_VARIABLE, 1)) memory
  in assertWithMessage result expected "Should increment variable. "

test_INC_STACK =
  let memory = defaultMemoryMap { vars = [0], stack = [1,2,3] }
      expected = memory { vars = [0], stack = [2,2,3], programCounter = 1 }
      result = processOpCode (INC (STORE_VARIABLE, 0)) memory
  in assertWithMessage result expected "Should increment head of stack. "

test_DEC =
  let memory = defaultMemoryMap { vars = [100] }
      expected = memory { vars = [99], programCounter = 1 }
      result = processOpCode (DEC (STORE_VARIABLE, 1)) memory
  in assertWithMessage result expected "Should decrement variable."

test_DEC_CHK =
  let memory = defaultMemoryMap { memory = V.fromList [0,0,0,0,0,0], vars = [100] }
      expected = memory { vars = [99], programCounter = 5 }
      result = processOpCode (DEC_CHK (STORE_VARIABLE, 1) (SMALL, 100) (BRANCH_OFFSET, 5)) memory
  in assertWithMessage result expected "Should decrement variable and jump."

test_DEC_STACK =
  let memory = defaultMemoryMap { vars = [0], stack = [3,2,1] }
      expected = memory { vars = [0], stack = [2,2,1], programCounter = 1 }
      result = processOpCode (DEC (STORE_VARIABLE, 0)) memory
  in assertWithMessage result expected "Should decrement head of stack."

test_JUMP =
  let memory = defaultMemoryMap { programCounter = 132}
      expected = memory { programCounter = 232 }
      result = processOpCode (JUMP (BRANCH_OFFSET, 100)) memory
  in assertWithMessage result expected "Should jump program counter by 100."

test_JUMP_negative =
  let memory = defaultMemoryMap { programCounter = 132}
      expected = memory { programCounter = 32 }
      result = processOpCode (JUMP (BRANCH_OFFSET, -100)) memory
  in assertWithMessage result expected "Should jump program counter by -100."

test_JUMP_zero =
  let memory = defaultMemoryMap { programCounter = 132}
      expected = memory
      result = processOpCode (JUMP (BRANCH_OFFSET, 0)) memory
  in assertWithMessage result expected "Should leave program counter unchanged."

--test_PIRACY =
  --let memory = defaultMemoryMap { programCounter = 132}
      --expected = memory { programCounter = 142 }
      --result = processOpCode PIRACY memory
  --in assertWithMessage result expected "Interpreter should not treat games as pirated"

test_JZ_zero =
  let memory = defaultMemoryMap { programCounter = 132}
      expected = memory { programCounter = 142 }
      result = processOpCode (JZ (SMALL, 0) (BRANCH_OFFSET, 10)) memory
  in assertWithMessage result expected "should jump since passed zero"

test_JZ_notzero =
  let memory = defaultMemoryMap { programCounter = 132}
      expected = memory { programCounter = 133 }
      result = processOpCode (JZ (SMALL, 10) (BRANCH_OFFSET, 10)) memory
  in assertWithMessage result expected "should not jump since passed ten"

test_JL_lessthan =
  let memory = defaultMemoryMap { programCounter = 132}
      expected = memory { programCounter = 142 }
      result = processOpCode (JL (SMALL, 12) (SMALL, 15) (BRANCH_OFFSET, 10)) memory
  in assertWithMessage result expected "should jump since a less than b"

test_JL_equal =
  let memory = defaultMemoryMap { programCounter = 132}
      expected = memory { programCounter = 133 }
      result = processOpCode (JL (SMALL, 15) (SMALL, 15) (BRANCH_OFFSET, 10)) memory
  in assertWithMessage result expected "should not jump since a equals b"

test_JL_greater =
  let memory = defaultMemoryMap { programCounter = 132}
      expected = memory { programCounter = 133 }
      result = processOpCode (JL (SMALL, 20) (SMALL, 15) (BRANCH_OFFSET, 10)) memory
  in assertWithMessage result expected "should not jump since a greater than b"

test_JG_lessthan =
  let memory = defaultMemoryMap { programCounter = 132}
      expected = memory { programCounter = 133 }
      result = processOpCode (JG (SMALL, 12) (SMALL, 15) (BRANCH_OFFSET, 10)) memory
  in assertWithMessage result expected "should not jump since a less than b"

test_JG_equal =
  let memory = defaultMemoryMap { programCounter = 132}
      expected = memory { programCounter = 133 }
      result = processOpCode (JG (SMALL, 15) (SMALL, 15) (BRANCH_OFFSET, 10)) memory
  in assertWithMessage result expected "should not jump since a equals b"

test_JG_greater =
  let memory = defaultMemoryMap { programCounter = 132}
      expected = memory { programCounter = 142 }
      result = processOpCode (JG (SMALL, 20) (SMALL, 15) (BRANCH_OFFSET, 10)) memory
  in assertWithMessage result expected "should jump since a greater than b"


test_JE_lessthan =
  let memory = defaultMemoryMap { programCounter = 132}
      expected = memory { programCounter = 133 }
      result = processOpCode (JE (SMALL, 12) (SMALL, 15) (BRANCH_OFFSET, 10)) memory
  in assertWithMessage result expected "should not jump since a less than b"

test_JE_equal =
  let memory = defaultMemoryMap { programCounter = 132}
      expected = memory { programCounter = 142 }
      result = processOpCode (JE (SMALL, 15) (SMALL, 15) (BRANCH_OFFSET, 10)) memory
  in assertWithMessage result expected "should jump since a equals b"

test_JE_greater =
  let memory = defaultMemoryMap { programCounter = 132}
      expected = memory { programCounter = 133 }
      result = processOpCode (JE (SMALL, 20) (SMALL, 15) (BRANCH_OFFSET, 10)) memory
  in assertWithMessage result expected "should not jump since a greater than b"


test_PRINT_ADDR_respect_term =
  let memory = defaultMemoryMap { memory = V.fromList [6342, 39110, 6342], shiftRegister = LOWER}
      expected = memory { stream1 = "aaaaaa", programCounter = 1}
      result = processOpCode (PRINT_ADDR (BRANCH_OFFSET, 0)) memory
  in assertWithMessage result expected "should append 'aaaaaa' to stream1, respecting the string terminator"

--test_PRINT_aaa =
  --let memory = defaultMemoryMap { memory = V.fromList [6342, 39110], shiftRegister = LOWER}
      --expected = memory { stream1 = "aaaaaa", programCounter = 1}
      --result = processOpCode (PRINT) memory
  --in assertWithMessage result expected "should append 'aaaaaa' to stream1"

test_PRINT_aaa =
  let memory = defaultMemoryMap { memory = V.fromList [0xB211, 6342, 39110], shiftRegister = LOWER}
      expected = memory { stream1 = "aaaaaa", programCounter = 1}
      result = processOpCode (PRINT) memory
  in assertWithMessage result expected "mine should append 'aaaaaa' to stream1"

test_PRINT_hello =
  let memory = defaultMemoryMap { memory = V.fromList [0xB211, 0xAA46, 0x3416, 0x459C, 0xA500], shiftRegister = LOWER}
      expected = memory { stream1 = "Hello.0", programCounter = 1}
      result = processOpCode (PRINT) memory
  in assertWithMessage result expected "should append 'Hello.0' to stream1"

test_ADD =
  let memory = defaultMemoryMap { vars = [0] }
      expected = memory { vars = [10], programCounter = 1}
      result = processOpCode (ADD (SMALL, 5) (SMALL, 5) (STORE_VARIABLE, 1)) memory
  in assertWithMessage result expected "should add 5 and 5 and store result in var 1"

test_ADD_2 =
  let memory = defaultMemoryMap { vars = [0,0] }
      expected = memory { vars = [0,100], programCounter = 1}
      result = processOpCode (ADD (SMALL, 50) (SMALL, 50) (STORE_VARIABLE, 2)) memory
  in assertWithMessage result expected "should add 50 and 50 and store result in var 2"

test_SUB =
  let memory = defaultMemoryMap { vars = [0] }
      expected = memory { vars = [10], programCounter = 1}
      result = processOpCode (SUB (SMALL, 15) (SMALL, 5) (STORE_VARIABLE, 1)) memory
  in assertWithMessage result expected "should subtract 15 and 5 and store result in var 1"

test_MUL =
  let memory = defaultMemoryMap { vars = [0] }
      expected = memory { vars = [50], programCounter = 1}
      result = processOpCode (MUL (SMALL, 10) (SMALL, 5) (STORE_VARIABLE, 1)) memory
  in assertWithMessage result expected "should multiply 10 and 5 and store result in var 1"


test_DIV =
  let memory = defaultMemoryMap { vars = [0] }
      expected = memory { vars = [2], programCounter = 1}
      result = processOpCode (DIV (SMALL, 10) (SMALL, 5) (STORE_VARIABLE, 1)) memory
  in assertWithMessage result expected "should divide 10 by 5 and store result in var 1"


test_DIV_notwhole =
  let memory = defaultMemoryMap { vars = [0] }
      expected = memory { vars = [1], programCounter = 1}
      result = processOpCode (DIV (SMALL, 10) (SMALL, 7) (STORE_VARIABLE, 1)) memory
  in assertWithMessage result expected "should divide 10 by 7 and get 1 and store result in var 1"

test_getOperandTypes_VARIABLE_FORM =
  let cells = [0x00, 0xF0, 0xFF]
      expected = [[LARGE, LARGE, LARGE, LARGE],
                  [LARGE, LARGE, OMITTED],
                  [OMITTED]]
      result = map (getOperandTypes VARIABLE_FORM) cells
  in assertWithMessage result expected "Testing getOperandTypes in variable form"

test_getOperandTypes_SHORT_FORM =
  let cell = [0,
              setBit 0 5,
              setBit 0 4,
              setBit 0 5 .|. setBit 0 4]
      expected = [[LARGE],
                  [SMALL],
                  [VARIABLE],
                  [OMITTED]]
      result = map (getOperandTypes SHORT_FORM) cell
  in assertWithMessage result expected "Testing getOperandTypes in short form"



test_getOperandType_SHORT_FORM =
  let bits = [(True, True),
              (True, False),
              (False,True),
              (False,False)]
      expected = [OMITTED, VARIABLE, SMALL, LARGE]
      result = map (uncurry (getOperandType SHORT_FORM)) bits
  in assertWithMessage result expected "Testing getOperandType"

test_getOpCode_0OP =
  let memory = [
        defaultMemoryMap { memory = V.fromList [0xB0] },
        defaultMemoryMap { memory = V.fromList [0xB1] },
        defaultMemoryMap { memory = V.fromList [0xB2] },
        defaultMemoryMap { memory = V.fromList [0xB3] },
        defaultMemoryMap { memory = V.fromList [0xB4] },
        defaultMemoryMap { memory = V.fromList [0xB5] },
        defaultMemoryMap { memory = V.fromList [0xB6] },
        defaultMemoryMap { memory = V.fromList [0xB7] },
        defaultMemoryMap { memory = V.fromList [0xB8] },
        defaultMemoryMap { memory = V.fromList [0xB9] },
        defaultMemoryMap { memory = V.fromList [0xBA] },
        defaultMemoryMap { memory = V.fromList [0xBB] },
        defaultMemoryMap { memory = V.fromList [0xBC] },
        defaultMemoryMap { memory = V.fromList [0xBD] },
        defaultMemoryMap { memory = V.fromList [0xBE] },
        defaultMemoryMap { memory = V.fromList [0xBF] }
        ]
      expected = [
        RTRUE,
        RFALSE,
        PRINT,
        PRINT_RET,
        NOP,
        SAVE,
        RESTORE,
        RESTART,
        RET_POPPED,
        POP,
        QUIT,
        NEW_LINE,
        SHOW_STATUS,
        VERIFY,
        NOP,
        PIRACY
        ]
      result = map getOpCode memory
  in assertWithMessage result expected "Testing getOpCode for 0OP "

--test_getOperand =
  --let cellBytes = [[0,0]]
      --expected = [(LARGE, 0)]
      --result   = map (uncurry (getOperand LARGE)) cellBytes
  --in  assertWithMessage result expected "Testing getOperand"

--test_getOperands =
  --let cellBytes = [[0,0,0,0]]
      --expected = [(LARGE, 0), (LARGE,0)]
      --result = map ((getOperands LARGE) cellBytes)
  --in assertWithMessage result expected "Testing getOperads"

--------- TEST CASES ----------
assertWithMessage result expected message =
  let messageL = message ++ "\n\t\tresult: " ++ show result ++ "\n\t\texpected: " ++ show expected
  in it messageL (result == expected)

assert :: Bool -> SpecWith ()
assert = it "Get off your butt and write a message!"

