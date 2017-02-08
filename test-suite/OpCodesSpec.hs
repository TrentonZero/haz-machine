-- test-suite/OpCodesSpec.hs
module OpCodesSpec
  (spec)
  where

import           Data.Maybe
import           Data.Vector.Unboxed
import           Debug.Trace
import           MemoryMap
import           OpCodes
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           ZSCIIString

spec :: Spec
spec =
  parallel $
  do describe "OpCodes Tests" $
       do test_QUIT
          test_pop
          test_nop
          test_newLine
          test_INC
          test_INC_STACK
          test_DEC
          test_DEC_STACK
          test_JUMP
          test_JUMP_negative
          test_JUMP_zero
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
  let memory = defaultMemoryMap { memory = fromList [1,2,3,4,5,6,7,0xFFFF,8,9,10] }
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
  let memory = defaultMemoryMap { memory = fromList [6342, 39110], shiftRegister = LOWER}
      expected = memory { stream1 = "aaaaaa", programCounter = 1}
      result = processOpCode (PRINT_ADDR 0) memory
  in assertWithMessage result expected "should append 'aaaaaa' to stream1"

test_PRINT_ADDR_respect_term =
  let memory = defaultMemoryMap { memory = fromList [6342, 39110, 6342], shiftRegister = LOWER}
      expected = memory { stream1 = "aaaaaa", programCounter = 1}
      result = processOpCode (PRINT_ADDR 0) memory
  in assertWithMessage result expected "should append 'aaaaaa' to stream1, respecting the string terminator"

test_PRINT_aaa =
  let memory = defaultMemoryMap { memory = fromList [6342, 39110], shiftRegister = LOWER}
      expected = memory { stream1 = "aaaaaa", programCounter = 1}
      result = processOpCode (PRINT [6,6,6,6,6,6]) memory
  in assertWithMessage result expected "should append 'aaaaaa' to stream1"

--------- TEST CASES ----------
assertWithMessage result expected message =
  let messageL = message Prelude.++ "\n\tresult: " Prelude.++ show result Prelude.++ "\n\texpected: " Prelude.++ show expected
  in it messageL $ do (result == expected)

assert :: Bool -> SpecWith ()
assert condition = it "Get off your butt and write a message!" $ do condition

