{-# LANGUAGE QuasiQuotes #-}

module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit (Assertion, (@?=))

import qualified Data.ByteString.Char8 as B
import Data.Char (isSpace)
import Data.Loc (SrcLoc, noLoc, startPos)
import Control.Exception (SomeException)
import Language.C.Quote.C
import qualified Language.C.Quote.GCC as GCC
import qualified Language.C.Syntax as C
import qualified Language.C.Parser as P
import MainCPP
import Numeric (showHex)
import GCC (gccTests)
import Objc (objcTests, objcRegressionTests)
import CUDA (cudaTests)
import Text.PrettyPrint.Mainland
import Text.PrettyPrint.Mainland.Class

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [ constantTests
        , constantAntiquotationsTests
        , cQuotationTests
        , cPatternAntiquotationTests
        , statementCommentTests
        , regressionTests
        , gccTests
        , objcTests
        , objcRegressionTests
        , cudaTests
        ]

constantTests :: Test
constantTests = testGroup "Constants"
    [ testCase "octal constant" test_octint
    , testCase "hex constant" test_hexint
    , testCase "unsigned hex constant" test_hexint_u
    , testCase "unsigned long hex constant" test_hexint_ul
    , testCase "unsigned long long hex constant hexint" test_hexint_ull
    ]
  where
    test_octint :: Assertion
    test_octint =
        [cexp|010|]
          @?= C.Const (C.IntConst "010" C.Signed 8 noLoc) noLoc

    test_hexint :: Assertion
    test_hexint =
        [cexp|0x10|]
          @?= C.Const (C.IntConst "0x10" C.Signed 16 noLoc) noLoc

    test_hexint_u :: Assertion
    test_hexint_u =
        [cexp|0x10U|]
          @?= C.Const (C.IntConst "0x10U" C.Unsigned 16 noLoc) noLoc

    test_hexint_ul :: Assertion
    test_hexint_ul =
        [cexp|0x10UL|]
          @?= C.Const (C.LongIntConst "0x10UL" C.Unsigned 16 noLoc) noLoc

    test_hexint_ull :: Assertion
    test_hexint_ull =
        [cexp|0x10ULL|]
          @?= C.Const (C.LongLongIntConst "0x10ULL" C.Unsigned 16 noLoc) noLoc

constantAntiquotationsTests :: Test
constantAntiquotationsTests = testGroup "Constant antiquotations" $
    [ testCase "int antiquotes" test_int
    , testCase "hex Const antiquote" test_hexconst
    , testCase "unsigned hex Const antiquote" test_hexconst_u
    , testGroup "float antiquotes" floatConstTests
    , testCase "char antiquote" test_char
    , testCase "string antiquote" test_string
    , testGroup "misc char constants" charConstTests
    ]
    ++ testCase_test_int_hsexp
  where
    test_int :: Assertion
    test_int =
        [cexp|$int:one + $uint:one + $lint:one + $ulint:one + $llint:one + $ullint:one|]
          @?= [cexp|1 + 1U + 1L + 1UL + 1LL + 1ULL|]
      where
        one :: Integer
        one = 1

    test_hexconst :: Assertion
    test_hexconst =
        [cexp|$const:(hexconst (10 :: Integer))|]
          @?= C.Const (C.IntConst "0xa" C.Signed 10 noLoc) noLoc
      where
        hexconst :: Integral a => a -> C.Const
        hexconst i = C.IntConst ("0x" ++ showHex x "") C.Signed x noLoc
          where
            x :: Integer
            x = fromIntegral i

    test_hexconst_u :: Assertion
    test_hexconst_u =
        [cexp|$const:(hexconst_u (10 :: Integer))|]
          @?= C.Const (C.IntConst "0xa" C.Unsigned 10 noLoc) noLoc
      where
        hexconst_u :: Integral a => a -> C.Const
        hexconst_u i = C.IntConst ("0x" ++ showHex x "") C.Unsigned x noLoc
          where
            x :: Integer
            x = fromIntegral i

    floatConstTests :: [Test]
    floatConstTests = [ testCase "float antiquotes" test_float
                      , testCase "NaN" test_NaN
                      , testCase "Infinity" test_infinity
                      ]
      where
        test_float :: Assertion
        test_float =
            [cexp|$float:one + $double:one + $ldouble:one|]
              @?= [cexp|1.0F + 1.0 + 1.0L|]
          where
            one :: Fractional a => a
            one = 1

        test_NaN :: Assertion
        test_NaN =
            showCompact [cexp|$double:nan|] @?= "NAN"
          where
            nan :: RealFloat a => a
            nan = acos 2

        test_infinity :: Assertion
        test_infinity =
            showCompact [cexp|$double:inf|] @?= "INFINITY"
          where
            inf :: RealFloat a => a
            inf = 1/0

    test_char :: Assertion
    test_char =
        [cexp|$char:a|] @?= [cexp|'a'|]
      where
        a = 'a'

    test_string :: Assertion
    test_string =
        [cexp|$string:hello|] @?= [cexp|"Hello, world\n"|]
      where
        hello = "Hello, world\n"

    charConstTests :: [Test]
    charConstTests = [ charConstTest '\0' "'\\0'"
                     , charConstTest '\xfff' "'\\u0fff'"
                     , charConstTest '\xfffff' "'\\U000fffff'"
                     ]
      where
        charConstTest :: Char -> String -> Test
        charConstTest c s =
          testCase ("character constant " ++ show c) $
          showCompact [cexp|$char:c|] @?= s

cQuotationTests :: Test
cQuotationTests = testGroup "C quotations"
    [ testCase "raw expression-level escape" test_escexp
    , testCase "raw statement-level escape" test_escstm
    , testCase "identifier antiquote" test_id
    , testCase "expression antiquote" test_exp
    , testCase "function antiquote" test_func
    , testCase "args antiquote" test_args
    , testCase "declaration antiquote" test_decl
    , testCase "struct declaration antiquote" test_sdecl
    , testCase "external declaration antiquote" test_edecl
    , testCase "enum antiquote" test_enum
    , testCase "statement antiquote" test_stm
    , testCase "parameter antiquote" test_param
    , testCase "type qualifier antiquote" test_tyqual
    , testCase "type qualifiers antiquote" test_tyquals
    , testCase "type antiquote" test_ty
    , testCase "initializer antiquote" test_init
    , testCase "initializers antiquote" test_inits
    , testCase "block items antiquote" test_item
    , testCase "qualifier with type antiquote 1" test_qual_antitype1
    , testCase "qualifier with type antiquote 2" test_qual_antitype2
    ]
  where
    test_id :: Assertion
    test_id =
        [cexp|$id:f($id:x, $id:y)|] @?= [cexp|f(x, y)|]
      where
        f :: String
        f = "f"

        x :: SrcLoc -> C.Id
        x = C.Id "x"

        y :: C.Id
        y = C.Id "y" noLoc

    test_escexp :: Assertion
    test_escexp =
        let rawstr = "a rather random string"
        in [cexp|$esc:rawstr|]
               @?= C.EscExp rawstr noLoc

    test_escstm :: Assertion
    test_escstm =
        let rawstr = "a rather random string"
        in [citems|$escstm:rawstr if (1) return;|]
               @?= [ C.BlockStm $ C.EscStm rawstr noLoc
                   , C.BlockStm $ C.If (C.Const (C.IntConst "1" C.Signed 1 noLoc) noLoc) (C.Return Nothing noLoc) Nothing noLoc ]

    test_exp :: Assertion
    test_exp =
        [cexp|$exp:e1 + $exp:e2|] @?= [cexp|1 + 2|]
      where
        e1 = [cexp|1|]
        e2 = [cexp|2|]

    test_func :: Assertion
    test_func =
        [cunit|$func:f|]
          @?= [cunit|int add(int x) { return x + 10; }|]
      where
        f = add (10 :: Integer)
        add n = [cfun|int add(int x) { return x + $int:n; } |]

    test_args :: Assertion
    test_args =
        [cstm|f($exp:e1, $args:args, $exp:e2);|]
          @?= [cstm|f(1, 2, 3, 4);|]
      where
        e1 = [cexp|1|]
        e2 = [cexp|4|]
        args = [[cexp|2|], [cexp|3|]]

    test_decl :: Assertion
    test_decl =
        [cfun|int inc(int n) {
                 $decl:d1;
                 $decls:decls

                 return n + 1;
              }|]
           @?= [cfun|int inc(int n) {
                         int i;
                         int j;
                         char c = 'c';

                         return n + 1;
                      }|]
      where
        d1 = [cdecl|int i;|]
        d2 = [cdecl|int j;|]
        d3 = [cdecl|char c = 'c';|]
        decls = [d2, d3]

    test_sdecl :: Assertion
    test_sdecl =
        [cty|struct foo { $sdecl:d1 $sdecls:decls }|]
          @?= [cty|struct foo { int i; int j; char c; }|]
      where
        d1 = [csdecl|int i;|]
        d2 = [csdecl|int j;|]
        d3 = [csdecl|char c;|]
        decls = [d2, d3]

    test_edecl :: Assertion
    test_edecl =
        [cunit|$edecl:d1 $edecls:decls|]
          @?= [cunit|int i; int j; char c = 'c';|]
      where
        d1 = [cedecl|int i;|]
        d2 = [cedecl|int j;|]
        d3 = [cedecl|char c = 'c';|]
        decls = [d2, d3]

    test_enum :: Assertion
    test_enum =
        [cty|enum foo { $enum:enum1, $enums:enums }|]
          @?= [cty|enum foo { A = 0, B, C = 2 }|]
      where
        enum1 = [cenum|A = 0|]
        enum2 = [cenum|B|]
        enum3 = [cenum|C = 2|]
        enums = [enum2, enum3]

    test_stm :: Assertion
    test_stm =
        [cfun|int add(int x) { $stms:stms return x + 1; }|]
          @?= [cfun|int add(int x) { a = 1; b = 2; return x + 1; }|]
      where
        one :: Integer
        one = 1
        stm1 = [cstm|a = $int:one;|]
        stm2 = [cstm|b = 2;|]
        stms = [stm1, stm2]

    test_param :: Assertion
    test_param =
        [cdecl|int f($param:ty1, $params:tys);|]
          @?= [cdecl|int f(char, int, float);|]
      where
        ty1 = [cparam|char|]
        ty2 = [cparam|int|]
        ty3 = [cparam|float|]
        tys = [ty2, ty3]

    test_tyqual :: Assertion
    test_tyqual =
        [cdecl|$tyqual:tyqual int i;|]
          @?= [cdecl|const int i;|]
      where
        tyqual = C.Tconst noLoc

    test_tyquals :: Assertion
    test_tyquals =
        [cdecl|$tyquals:tyquals int i;|]
          @?= [cdecl|const volatile int i;|]
      where
        tyquals = [ctyquals|const volatile|]

    test_ty :: Assertion
    test_ty =
        [cdecl|$ty:ty1 f(const $ty:ty2);|]
          @?= [cdecl|int f(const float);|]
      where
        ty1 = [cty|int|]
        ty2 = [cty|float|]

    test_init :: Assertion
    test_init =
        [cinit|{$init:initializer, .a = 10}|] @?= [cinit|{{.d = 1}, .a = 10}|]
      where
        initializer = [cinit|{.d = 1}|]

    test_inits :: Assertion
    test_inits =
        [cinit|{$inits:([initializer1, initializer2])}|] @?= [cinit|{{.d = 1},{.a = 10}}|]
      where
        initializer1 = [cinit|{.d = 1}|]
        initializer2 = [cinit|{.a = 10}|]

    test_item :: Assertion
    test_item =
        [cfun|int add(int x) { int y = 2; return x + y; }|]
          @?= [cfun|int add(int x) { $items:([item1, item2]) }|]
      where
        item1 = [citem|int y = 2;|]
        item2 = [citem|return x + y;|]

    test_qual_antitype1 :: Assertion
    test_qual_antitype1 =
        [cexp|(const $ty:tau) NULL|]
          @?= [cexp|(const int) NULL|]
      where
        tau = [cty|int|]

    test_qual_antitype2 :: Assertion
    test_qual_antitype2 =
        [cexp|(const $ty:tau *) NULL|]
          @?= [cexp|(const int*) NULL|]
      where
        tau = [cty|int|]

cPatternAntiquotationTests :: Test
cPatternAntiquotationTests = testGroup "C pattern antiquotations"
    [ testCase "arguments pattern antiquote" pat_args
    ]
  where
    pat_args :: Assertion
    pat_args =
        stms @?= [[cexp|2|], [cexp|3|]]
      where
        stms = case [cstm|f(1, 2, 3);|] of
                 [cstm|f(1, $args:es);|] -> es
                 _ -> []

statementCommentTests :: Test
statementCommentTests = testGroup "Statement comments"
    [ testCase "lbrace comment" test_lbrace_comment
    , testCase "semi comment" test_semi_comment
    , testCase "c comment" test_c_comment
    , testCase "c++ comment" test_cxx_comment
    , testCase "antiquote comment" test_antiquote_comment
    , testCase "comment at end of statements quote" test_stms_end_comment
    , testCase "comment before antiquoted statements" test_block_stms_comment
    , testCase "comment at beginning of a block" test_issue_55
    , testCase "comment inside cunit block" test_issue_76
    ]
  where
    test_lbrace_comment :: Assertion
    test_lbrace_comment =
        [cstm|{ $comment:("/* Test 1 */") return x + y; }|]
          @?= [cstm|{/* Test 1 */ return x + y; }|]

    test_semi_comment :: Assertion
    test_semi_comment =
        [cstms|x = 1; $comment:("/* Test 1 */") return x + y;|]
          @?= [cstms|x = 1; /* Test 1 */ return x + y;|]

    assign_a_equals_one =
      C.Exp (Just $ C.Assign (C.Var (C.Id "a" noLoc) noLoc)
                              C.JustAssign
                              (C.Const (C.IntConst "1" C.Signed 1 noLoc) noLoc)
                              noLoc)
            noLoc

    test_c_comment =
        [cstms|
        a = 1;
        /* c style comment */
        |]

        @?= [ assign_a_equals_one
            , C.Comment "/* c style comment */" (C.Exp Nothing noLoc) noLoc
            ]

    test_cxx_comment =
        [cstms|
        a = 1;
        // c++ style comment
        |]

        @?= [ assign_a_equals_one
            , C.Comment "// c++ style comment" (C.Exp Nothing noLoc) noLoc
            ]

    test_antiquote_comment =
        [cstms|
        $comment:("/* antiquote comment */")
        |]

        @?= [ C.Comment "/* antiquote comment */" (C.Exp Nothing noLoc) noLoc
            ]

    test_stms_end_comment :: Assertion
    test_stms_end_comment =
        [cstms|x = 1; return x + y; $comment:("// Test")|]
          @?= [cstms|x = 1; return x + y; // Test|]

    test_block_stms_comment :: Assertion
    test_block_stms_comment =
        [cstm|{ int a; $decl:decl; /* Test */ $stms:stms }|]
          @?= [cstm|{ int a; int b; a = 1; b = 2;}|]
      where
        decl = [cdecl|int b;|]
        stm1 = [cstm|a = 1;|]
        stm2 = [cstm|b = 2;|]
        stms = [stm1, stm2]

    test_issue_55 :: Assertion
    test_issue_55 =
        [cunit|int f(int x)
              { // Breaking comment.
                int y;
                return x;
              }|]
        @?=
        [cunit|int f(int x)
              { $comment:("// Breaking comment.")
                int y;
                return x;
              }|]

    test_issue_76 :: Assertion
    test_issue_76 =
        [cunit|
          $edecl:d1
          /* AAA */
          $edecl:d2
          struct A { int foo; };
          /* BBB */
          struct B { int bar; };
        |]
        @?=
        [cunit|
          int i;
          /* AAA */
          int j;
          struct A { int foo; };
          $comment:(" BBB ")
          struct B { int bar; };
        |]
      where
        d1 = [cedecl|int i;|]
        d2 = [cedecl|int j;|]

regressionTests :: Test
regressionTests = testGroup "Regressions"
    [ issue68
    , issue64
    , testCase "pragmas" test_pragmas
    , issue48
    , testCase "Issue #44" issue44
    , issue43
    ]
  where
    test_pragmas :: Assertion
    test_pragmas =
        [cstms|
        #pragma omp sections
        {
            #pragma omp section
            a = 1;
        }
        |]

        @?= [ C.Pragma "omp sections" noLoc
            , C.Block [ C.BlockStm (C.Pragma "omp section" noLoc)
                      , C.BlockStm (C.Exp (Just $ C.Assign (C.Var (C.Id "a" noLoc) noLoc)
                                                           C.JustAssign
                                                           (C.Const (C.IntConst "1" C.Signed 1 noLoc) noLoc)
                                                           noLoc)
                                          noLoc)
                      ]
                      noLoc
            ]

    issue68 :: Test
    issue68 = testCase "Issue #68"$
        showCompact [cstm|if (!initialized) { $stms:init_stms }|]
        @?=
        "if (!initialized) { return; }"
      where
        init_stms :: [C.Stm]
        init_stms = [[cstm|return;|]]

    issue64 :: Test
    issue64 = testGroup "Issue #64"
              [ testCase "-($int:i)"  test_issue64_1
              , testCase "--($int:i)" test_issue64_2
              ]
      where
        i :: Int
        i = -42

        test_issue64_1 :: Assertion
        test_issue64_1 = pretty 80 (ppr [cexp|-($int:i)|]) @?= "-(-42)"

        test_issue64_2 :: Assertion
        test_issue64_2 = pretty 80 (ppr [cexp|--$int:i|]) @?= "--(-42)"

    issue48 :: Test
    issue48 = testGroup "Issue #48"
              [ testCase "-(-42)"  test_issue48_1
              , testCase "--(-42)" test_issue48_2
              , testCase "-(--42)" test_issue48_3
              , testCase "+(+42)"  test_issue48_4
              , testCase "++(+42)" test_issue48_5
              , testCase "+(++42)" test_issue48_6
              ]
      where
        test_issue48_1 :: Assertion
        test_issue48_1 = pretty 80 (ppr [cexp|-(-42)|]) @?= "-(-42)"

        test_issue48_2 :: Assertion
        test_issue48_2 = pretty 80 (ppr [cexp|--(-42)|]) @?= "--(-42)"

        test_issue48_3 :: Assertion
        test_issue48_3 = pretty 80 (ppr [cexp|-(--42)|]) @?= "-(--42)"

        test_issue48_4 :: Assertion
        test_issue48_4 = pretty 80 (ppr [cexp|+(+42)|]) @?= "+(+42)"

        test_issue48_5 :: Assertion
        test_issue48_5 = pretty 80 (ppr [cexp|++(+42)|]) @?= "++(+42)"

        test_issue48_6 :: Assertion
        test_issue48_6 = pretty 80 (ppr [cexp|+(++42)|]) @?= "+(++42)"

    issue44 :: Assertion
    issue44 =
        case parseDecl "$ty:something c;" of
          Left err  -> fail (show err)
          Right grp -> (pretty 80 . ppr) grp @?= "$ty:something c"
      where
        parseDecl :: String -> Either SomeException C.InitGroup
        parseDecl s = P.parse [C.Antiquotation] [] P.parseDecl (B.pack s) (Just (startPos "<inline>"))

    issue43 :: Test
    issue43 = testGroup "Issue #43"
              [ testCase "float _Complex" test_issue43_1
              , testCase "long double _Complex" test_issue43_2
              , testCase "long _Complex double" test_issue43_3
              , testCase "_Imaginary long double" test_issue43_4
              ]
      where
        test_issue43_1 :: Assertion
        test_issue43_1 = [cty|float _Complex|] @?=
                         C.Type (C.DeclSpec [] [] (C.Tfloat_Complex noLoc) noLoc)
                                (C.DeclRoot noLoc)
                                noLoc

        test_issue43_2 :: Assertion
        test_issue43_2 = [cty|long double _Complex|] @?=
                         C.Type (C.DeclSpec [] [] (C.Tlong_double_Complex noLoc) noLoc)
                                (C.DeclRoot noLoc)
                                noLoc

        test_issue43_3 :: Assertion
        test_issue43_3 = [cty|long  _Complex double|] @?=
                         C.Type (C.DeclSpec [] [] (C.Tlong_double_Complex noLoc) noLoc)
                                (C.DeclRoot noLoc)
                                noLoc

        test_issue43_4 :: Assertion
        test_issue43_4 = [cty|_Imaginary long double|] @?=
                         C.Type (C.DeclSpec [] [] (C.Tlong_double_Imaginary noLoc) noLoc)
                                (C.DeclRoot noLoc)
                                noLoc

-- | Pretty-print a value on single line.
showCompact :: Pretty a => a -> String
showCompact = map space2space . flip displayS "" . renderCompact . ppr
  where
    space2space :: Char -> Char
    space2space c | isSpace c = ' '
                  | otherwise = c
