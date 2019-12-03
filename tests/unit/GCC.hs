{-# LANGUAGE QuasiQuotes #-}

module GCC (
    gccTests
  ) where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit (Assertion, (@?=))

import qualified Data.ByteString.Char8 as B
import Data.Char (isSpace)
import Data.Loc (SrcLoc, noLoc, startPos)
import Control.Exception (SomeException)
import Language.C.Quote.GCC
import qualified Language.C.Syntax as C
import qualified Language.C.Parser as P
import Text.PrettyPrint.Mainland
import Text.PrettyPrint.Mainland.Class

gccTests :: Test
gccTests = testGroup "GCC attribute quotations"
    [ testCase "attr antiquote" test_attr
    , testCase "attrs antiquote" test_attrs
    , testCase "attrs antiquote pretty" test_attr_p
    ]
  where
    test_attr :: Assertion
    test_attr =
         [cedecl| int test __attribute__(($attr:a,$attr:b));|]
           @?=
         [cedecl| int test __attribute__((section(".sram2"), noinit));|]
      where
        a = [cattr| section(".sram2") |]
        b = [cattr| noinit |]

    test_attrs :: Assertion
    test_attrs =
         [cedecl| int test __attribute__(($attrs:as));|]
           @?=
         [cedecl| int test __attribute__((section(".sram2"), noinit));|]
      where
        a = [cattr| section(".sram2") |]
        b = [cattr| noinit |]
        as = [ a, b ]

    test_attr_p :: Assertion
    test_attr_p =
      pretty 80 (ppr [cattr|section(".sram2")|]) @?= "section(\".sram2\")"
