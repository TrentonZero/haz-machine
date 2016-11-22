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
  let memory = MemoryMap (fromList [1,2,3,4,5,6,7,0xFFFF,8,9,10]) [] 0 [] LOWER False []
      expected = True
  in assertWithMessage (shouldTerminate (processOpCode QUIT memory) == expected)
                       "Should set the terminate flag so that MAIN IO Can kill it"


--------- TEST CASES ----------
assertWithMessage
  :: Bool -> String -> SpecWith ()
assertWithMessage condition message = it message $ do condition

assert :: Bool -> SpecWith ()
assert condition = it "Get off your butt and write a message!" $ do condition

