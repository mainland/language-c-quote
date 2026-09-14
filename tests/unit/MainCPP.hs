{-# LANGUAGE CPP         #-}
{-# LANGUAGE QuasiQuotes #-}

-- This module is needed because it's not possible to turn on CPP in Main.hs.
-- We need CPP because this test case doesn't work without FULL_HASKELL_ANTIQUOTES
-- turned on. (The simpler Haskell parser doesn't support infix operators.)

module MainCPP where

import           Test.Tasty
#ifdef FULL_HASKELL_ANTIQUOTES
import           Test.Tasty.HUnit

import           Language.C.Quote.C
#endif

testCase_test_int_hsexp :: [TestTree]
#ifdef FULL_HASKELL_ANTIQUOTES
testCase_test_int_hsexp =
    [testCase "unsigned long antiquote of Haskell expression" test_int_hsexp]
  where
    test_int_hsexp :: Assertion
    test_int_hsexp =
        [cexp|$ulint:(13 - 2*5 :: Integer)|] @?= [cexp|3UL|]
#else
testCase_test_int_hsexp =
    []
#endif
