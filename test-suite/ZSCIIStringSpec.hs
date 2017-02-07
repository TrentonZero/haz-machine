-- test-suite/ZSCIIStringSpec.hs
module ZSCIIStringSpec
  (spec)
  where
import           Data.Maybe
import           Data.Vector.Unboxed
import           Debug.Trace
import           MemoryMap
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           ZSCIIString

spec :: Spec
spec =
  parallel $
  do describe "ZSCIIString Tests" $
       do test_readZSCIIString_base
          test_readZSCIIString_noterm
          test_readASCIIString_base
          test_splitMemoryCellToZChar
          test_splitMemoryCellToZChar_with_term
          test_convertZCharToASCIIChar_lower
          test_convertZCharToASCIIChar_upper
          test_convertZCharToASCIIChar_symbol
          test_convertZCharToASCIIChar_lower_shiftUpper
          test_convertZCharToASCIIChar_lower_shiftSymbol
          test_convertZCharToASCIIChar_upper_shiftSymbol
          test_convertZCharToASCIIChar_upper_shiftLower
          test_convertZCharToASCIIChar_symbol_shiftLower
          test_convertZCharToASCIIChar_symbol_shiftUpper


test_readZSCIIString_base =
  let memory = defaultMemoryMap { memory = (fromList [1,2,3,4,5,6,7,0xFFFF,8,9,10]), shiftRegister = LOWER}
      location = 2
      expected_memory = [3,4,5,6,7,0xFFFF]
  in assertWithMessage ((readZSCIIString memory location) == expected_memory)
                       "Read ZCSII string from memory"

test_readZSCIIString_noterm =
  let memory = defaultMemoryMap { memory = (fromList [1..10]), shiftRegister= LOWER}
      location = 2
      expected_memory = [3..10]
  in assertWithMessage (readZSCIIString memory location == expected_memory)
                       "Read ZCSII string from memory when terminator is missing"


test_readASCIIString_base =
  let memory = defaultMemoryMap { memory = (fromList [6342,39110]), shiftRegister = LOWER}
      location = 0
      expected = "aaaaaa"  -- 6342 is "aaa" and 39110 is "aaa" with string term. need to compile a better sample
  in assertWithMessage ((readASCIIString memory location) == expected)
                       "Read ZCSII string from memory and parse into ASCII"


test_splitMemoryCellToZChar =
  let cell = 6342  -- 6342 = 00110-00110-00110, or 6 6 6
      expected = [6,6,6]
  in assertWithMessage (splitMemoryCellToZChar cell == expected)
                       "Split a single memory cell into 3 zchar"

test_splitMemoryCellToZChar_with_term =
  let cell = 39110  -- 39110 = 10110-00110-00110, or 6 6 6 with line terminator
      expected = [6,6,6]
  in assertWithMessage (splitMemoryCellToZChar cell == expected)
                       "Split a single memory cell into 3 zchar when last char has a line terminator"




test_convertZCharToASCIIChar_lower =
  let zchar =
        [6..31]
      state = (defaultMemoryMap { shiftRegister = LOWER}, Nothing)
      expected = "abcdefghijklmnopqrstuvwxyz"
  in assertWithMessage (Prelude.map fromJust (Prelude.map snd (Prelude.map (convertZCharToASCIICharGivenState state) zchar)) == expected) "Convert zchar to ascii in lower case"

test_convertZCharToASCIIChar_upper =
  let zchar =
        [6..31]
      state = (defaultMemoryMap { shiftRegister = UPPER}, Nothing)
      expected = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  in assertWithMessage
       (Prelude.map
          fromJust
          (Prelude.map
             snd
             (Prelude.map (convertZCharToASCIICharGivenState state)
                          zchar)) ==
        expected)
       "Convert zchar to ascii in upper case"

test_convertZCharToASCIIChar_symbol =
  let zchar =
        [6..31]
      state = (defaultMemoryMap { shiftRegister = SYMBOL}, Nothing)
      expected = " 0123456789.,!?_#'\"/\\<-:()"
  in assertWithMessage
       (Prelude.map
          fromJust
          (Prelude.map
             snd
             (Prelude.map (convertZCharToASCIICharGivenState state)
                          zchar)) ==
        expected)
       "Convert zchar to ascii in symbol case"

test_convertZCharToASCIIChar_lower_shiftUpper =
  let zchar =  [2,6,6]
      state = defaultMemoryMap {shiftRegister = LOWER}
      expected = [Nothing, Just 'A', Just 'a']
  in assertWithMessage ((evaluateZString state zchar) == expected)
                       "Convert zchar to ascii with upper case shift"

test_convertZCharToASCIIChar_lower_shiftSymbol =
  let zchar =  [3,6,6]
      state = defaultMemoryMap {shiftRegister = LOWER}
      expected = [Nothing, Just ' ', Just 'a']
  in assertWithMessage ( (evaluateZString state zchar) == expected)
                       "Convert zchar to ascii with symbol case shift"

test_convertZCharToASCIIChar_upper_shiftSymbol =
  let zchar =  [2,6,6]
      state = defaultMemoryMap {shiftRegister = UPPER}
      expected = [Nothing, Just ' ', Just 'A']
  in assertWithMessage ( (evaluateZString state zchar) == expected)
                       "Convert zchar to ascii with symbol case shift"

test_convertZCharToASCIIChar_upper_shiftLower =
  let zchar =  [3,6,6]
      state = defaultMemoryMap { shiftRegister = UPPER}
      expected = [Nothing, Just 'a', Just 'A']
  in assertWithMessage ( (evaluateZString state zchar) == expected)
                       "Convert zchar to ascii with lower case shift"


test_convertZCharToASCIIChar_symbol_shiftLower =
  let zchar =  [2,6,6]
      state = defaultMemoryMap { shiftRegister = SYMBOL}
      expected = [Nothing, Just 'a', Just ' ']
  in assertWithMessage ( (evaluateZString' state zchar) == expected)
                       "Convert zchar to ascii with lower case shift"

test_convertZCharToASCIIChar_symbol_shiftUpper =
  let zchar =  [3,6,6]
      state = defaultMemoryMap { shiftRegister = SYMBOL}
      expected = [Nothing, Just 'A', Just ' ']
  in assertWithMessage ( (evaluateZString state zchar) == expected)
                       "Convert zchar to ascii with upper case shift"




--------- TEST CASES ----------
assertWithMessage
  :: Bool -> String -> SpecWith ()
assertWithMessage condition message = it message condition

assert :: Bool -> SpecWith ()
assert = it "Get off your butt and write a message!"

