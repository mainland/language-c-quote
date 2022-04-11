{-# LANGUAGE QuasiQuotes #-}

module ISPC (ispcTests) where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit (Assertion, assert, (@?=))

import qualified Data.ByteString.Char8 as B
import Data.Char (isSpace)
import Data.Loc (SrcLoc, noLoc, startPos)
import Control.Exception (SomeException)
import Language.C.Quote.ISPC
import Language.C.Smart ()
import qualified Language.C.Syntax as C
import qualified Language.C.Parser as P
import Text.PrettyPrint.Mainland
import Text.PrettyPrint.Mainland.Class

ispcTests :: Test
ispcTests = testGroup "ISPC land"
    [ testCase "ispc unmasked" test_unmasked_qualifier
    ,testCase "ispc unmasked wvars" test_unmasked_qualifier_wvars
    ,testCase "ispc foreach" test_foreach
    ,testCase "ispc multidimensional foreach" test_foreach_multiple
    ,testCase "ispc foreach_active" test_foreach_active
    ,testCase "ispc foreach_tiled" test_foreach_tiled
    ,testCase "ispc multidimensional foreach_tiled" test_foreach_tiled_multiple
    ,testCase "ispc uniform" test_uniform
    ,testCase "ispc varying" test_varying
    ,testCase "ispc export" test_export
    ,testCase "ispc extern" test_extern
    ,testCase "ispc foreach_unique" test_foreach_unique
    ]

test_foreach :: Assertion
test_foreach = pretty 80 (ppr decl) @?= expected
  where
    decl = [cstm|foreach(a = 1 ... n){int a = 10;}|]
    expected = "foreach (a = 1 ... n) {\n    int a = 10;\n}"

test_foreach_multiple :: Assertion
test_foreach_multiple = pretty 80 (ppr decl) @?= expected
  where
    decl = [cstm|foreach(a = 1 ... n, b = 2 ... m){int a = 10;}|]
    expected = "foreach (a = 1 ... n, b = 2 ... m) {\n    int a = 10;\n}"
           
test_foreach_active :: Assertion
test_foreach_active = pretty 80 (ppr decl) @?= expected
  where
    decl = [cstm|foreach_active(index){int a = 10;}|]
    expected = "foreach_active (index) {\n    int a = 10;\n}"

test_foreach_tiled :: Assertion
test_foreach_tiled = pretty 80 (ppr decl) @?= expected
  where decl =  [cstm|foreach_tiled(a = 1 ...n){int a = 10;}|]
        expected = "foreach_tiled (a = 1 ... n) {\n    int a = 10;\n}"

test_foreach_tiled_multiple :: Assertion
test_foreach_tiled_multiple = pretty 80 (ppr decl) @?= expected
  where decl =  [cstm|foreach_tiled(a = 1 ...n, b= 2 ... m){int a = 10;}|]
        expected = "foreach_tiled (a = 1 ... n, b = 2 ... m) {\n    int a = 10;\n}"

test_foreach_unique :: Assertion
test_foreach_unique = pretty 80 (ppr decl) @?= expected
  where decl =  [cstm|foreach_unique(y in x){int a = 10;}|]
        expected = "foreach_unique (y in x) {\n    int a = 10;\n}"

test_unmasked_qualifier :: Assertion
test_unmasked_qualifier = pretty 80 (ppr decl) @?= expected
  where
    decl = [cedecl|extern "C" unmasked void func(int * uniform b, float c);|]
    expected = "extern \"C\" unmasked void func(int *uniform b, float c);" 

test_unmasked_qualifier_wvars :: Assertion
test_unmasked_qualifier_wvars = pretty 80 (ppr decl) @?= expected
  where
    decl = [cedecl|extern "C" unmasked void $id:a(int * uniform $id:b, float $id:c);|]
      where
        a = "func"
        b = "b"
        c = "c"
    expected = "extern \"C\" unmasked void func(int *uniform b, float c);" 

test_uniform :: Assertion
test_uniform = pretty 80 (ppr decl) @?= expected
  where
    decl = [cedecl|uniform int a = 10;|]
    expected = "uniform int a = 10;"

test_varying :: Assertion
test_varying = pretty 80 (ppr decl) @?= expected
  where
    decl = [cedecl|varying int a = 5;|]
    expected = "varying int a = 5;"

test_export :: Assertion
test_export = pretty 80 (ppr decl) @?= expected
  where
    decl = [cedecl|export int func(int a, float b){int c = 10;}|]
    expected = "export int func(int a, float b)\n{\n    int c = 10;\n}"

test_extern :: Assertion
test_extern = pretty 80 (ppr decl) @?= expected
  where
    decl = [cedecl|extern "C" void func(int a, float b);|]
    expected = "extern \"C\" void func(int a, float b);"
