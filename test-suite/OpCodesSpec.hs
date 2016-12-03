-- test-suite/OpCodesSpec.hs
module OpCodesSpec 
  (spec)
  where

import ZSCIIString
import MemoryMap
import OpCodes
import Test.Hspec
import Test.Hspec.QuickCheck
import Data.Vector.Unboxed
import Data.Maybe
import Debug.Trace

spec :: Spec
spec = 
  parallel $
  do describe "OpCodes Tests" $
       do test_QUIT
          test_advance_pc
          test_pop
          test_nop
          test_newLine
          test_INC
          test_INC_STACK
          test_DEC

test_nop =
  let memory = defaultMemoryMap
      expected = 1
  in assertWithMessage (programCounter (processOpCode NOP memory) == expected)
                       "Should advance the program counter and nothing else"


test_pop =
  let memory = defaultMemoryMap { stack = [1,2,3] }
      expected = [2,3]
  in assertWithMessage (stack (processOpCode POP memory) == expected)
                       "Should pop top value off the stack" 

test_newLine =
  let memory = defaultMemoryMap
      expected = "\n"
  in assertWithMessage (stream1 (processOpCode NEW_LINE memory) == expected)
                       "new line should advance the program counter"

test_advance_pc =
  let memory = defaultMemoryMap
      expected = 1
  in assertWithMessage (programCounter (processOpCode QUIT memory) == expected)
                       "Should advance the program counter"


test_QUIT =
  let memory = defaultMemoryMap { memory = fromList [1,2,3,4,5,6,7,0xFFFF,8,9,10] } 
      expected = True
  in assertWithMessage (shouldTerminate (processOpCode QUIT memory) == expected)
                       "Should set the terminate flag so that MAIN IO Can kill it"

test_INC =
  let memory = defaultMemoryMap { vars = [0] }
      expected = memory { vars = [1], programCounter = 1 }
      result = processOpCode (INC 1) memory
  in assertWithMessage (result == expected)
                       ("Should increment variable. \n\tresult: " Prelude.++ show result Prelude.++ " \n\texpected: " Prelude.++ show expected)

test_INC_STACK =
  let memory = defaultMemoryMap { vars = [0], stack = [1,2,3] }
      expected = memory { vars = [0], stack = [2,2,3], programCounter = 1 }
      result = processOpCode (INC 0) memory
  in assertWithMessage (result == expected)
                       ("Should increment head of stack. \n\tresult: " Prelude.++ show result Prelude.++ " \n\texpected: " Prelude.++ show expected)

test_DEC =
  let memory = defaultMemoryMap { vars = [100] }
      expected = memory { vars = [99], programCounter = 1 }
      result = processOpCode (DEC 1) memory
  in assertWithMessage (result == expected)
                       ("Should decrement variable. \n\tresult: " Prelude.++ show result Prelude.++ " \n\texpected: " Prelude.++ show expected)


test_DEC_STACK =
  let memory = defaultMemoryMap { vars = [0], stack = [3,2,1] }
      expected = memory { vars = [0], stack = [2,2,1], programCounter = 1 }
      result = processOpCode (DEC 0) memory
  in assertWithMessage (result == expected)
                       ("Should decrement head of stack. \n\tresult: " Prelude.++ show result Prelude.++ " \n\texpected: " Prelude.++ show expected)




--------- TEST CASES ----------
assertWithMessage
  :: Bool -> String -> SpecWith ()
assertWithMessage condition message = it message $ do condition

assert :: Bool -> SpecWith ()
assert condition = it "Get off your butt and write a message!" $ do condition

