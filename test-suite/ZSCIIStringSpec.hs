-- test-suite/ZSCIIStringSpec.hs
module ZSCIIStringSpec
  (spec)
  where

import ZSCIIString
import MemoryMap
import Test.Hspec
import Test.Hspec.QuickCheck
import Data.Vector.Unboxed
import Data.Maybe

spec :: Spec
spec = 
  parallel $
  do describe "ZSCIIString Tests" $
       do test_readZSCIIString_base
          test_readZSCIIString_noterm
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
  let memory = MemoryMap (fromList [1,2,3,4,5,6,7,0xFFFF,8,9,10]) [] 0 [] LOWER
      location = 2
      expected_memory = [3,4,5,6,7,0xFFFF]
  in assertWithMessage (readZSCIIString memory location == expected_memory)
                       "Read ZCSII string from memory"

test_readZSCIIString_noterm = 
  let memory = MemoryMap (fromList [1..10]) [] 0 [] LOWER
      location = 2
      expected_memory = [3..10]
  in assertWithMessage (readZSCIIString memory location == expected_memory)
                       "Read ZCSII string from memory when terminator is missing"

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
      state = (MemoryMap (fromList []) [] 0 [] LOWER, Nothing)
      expected = "abcdefghijklmnopqrstuvwxyz"
  in assertWithMessage (Prelude.map fromJust (Prelude.map snd (Prelude.map (convertZCharToASCIICharGivenState state) zchar)) == expected) "Convert zchar to ascii in lower case"

test_convertZCharToASCIIChar_upper = 
  let zchar = 
        [6..31]
      state = (MemoryMap (fromList []) [] 0 [] UPPER, Nothing)
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
      state = (MemoryMap (fromList []) [] 0 [] SYMBOL, Nothing)
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
  let zchar = fromList [2,6,6]
      state = (MemoryMap (fromList []) [] 0 [] LOWER)
      expected = "Aa"
  in assertWithMessage (snd (evaluateZString state zchar) == Just 'a')
                       "Convert zchar to ascii with upper case shift"

test_convertZCharToASCIIChar_lower_shiftSymbol = 
  let zchar = fromList [3,6,6]
      state = (MemoryMap (fromList []) [] 0 [] LOWER)
      expected = "Aa"
  in assertWithMessage (snd (evaluateZString state zchar) == Just 'a')
                       "Convert zchar to ascii with symbol case shift"

test_convertZCharToASCIIChar_upper_shiftSymbol = 
  let zchar = fromList [2,6,6]
      state = (MemoryMap (fromList []) [] 0 [] UPPER)
      expected = "Aa"
  in assertWithMessage (snd (evaluateZString state zchar) == Just 'a')
                       "Convert zchar to ascii with symbol case shift"

test_convertZCharToASCIIChar_upper_shiftLower = 
  let zchar = fromList [3,6,6]
      state = (MemoryMap (fromList []) [] 0 [] UPPER)
      expected = "Aa"
  in assertWithMessage (snd (evaluateZString state zchar) == Just 'a')
                       "Convert zchar to ascii with lower case shift"


test_convertZCharToASCIIChar_symbol_shiftLower = 
  let zchar = fromList [2,6,6]
      state = (MemoryMap (fromList []) [] 0 [] SYMBOL)
      expected = "Aa"
  in assertWithMessage (snd (evaluateZString state zchar) == Just 'a')
                       "Convert zchar to ascii with lower case shift"

test_convertZCharToASCIIChar_symbol_shiftUpper = 
  let zchar = fromList [3,6,6]
      state = (MemoryMap (fromList []) [] 0 [] SYMBOL)
      expected = "Aa"
  in assertWithMessage (snd (evaluateZString state zchar) == Just 'a')
                       "Convert zchar to ascii with upper case shift"


--- evaluateZString state zchar = let x = (Prelude.map snd (Prelude.map (convertZCharToASCIICharGivenState state) zchar))
---				in if (x == Nothing) Prelude.map fromJust 
evaluateZString state zchar = 
  foldl' convertZCharToASCIICharGivenState (state,Nothing) zchar

--------- TEST CASES ----------
assertWithMessage
  :: Bool -> String -> SpecWith ()
assertWithMessage condition message = it message $ do condition

assert :: Bool -> SpecWith ()
assert condition = it "Get off your butt and write a message!" $ do condition

