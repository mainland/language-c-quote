{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit ((@=?), listAssert)

import Language.Haskell.TH.Lib
import Language.Haskell.TH.Quote

import Language.C.Quote.C
import qualified Language.C.Quote.OpenCL as OCL
import System.Exit (exitFailure, exitSuccess)

#if !MIN_VERSION_template_haskell(2,7,0)
import qualified Data.Loc
import qualified Data.Symbol
import qualified Language.C.Syntax
#endif /* !MIN_VERSION_template_haskell(2,7,0) */

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [exp_id, exp_int, exp_float, exp_char, exp_string,
         exp_exp, exp_func, exp_args, exp_decl, exp_sdecl,
         exp_enum, exp_edecl, exp_stm, exp_param, exp_ty,
         pat_args, exp_hexp,
         exp_init, exp_inits, exp_item, ocl_ty]

exp_id :: Test
exp_id = testCase "exp id" $ [cexp|$id:ident|] @=? [cexp|x|]
  where
    ident = "x"

exp_int :: Test
exp_int = testCase "exp int" $
    [cexp|$int:one + $uint:one + $lint:one + $ulint:one|]
      @=? [cexp|1 + 1U + 1L + 1UL|]
  where
    one = 1

exp_float :: Test
exp_float = testCase "exp float" $
    [cexp|$float:one + $double:one + $ldouble:one|]
      @=? [cexp|1.0F + 1.0 + 1.0L|]
  where
    one = 1

exp_char :: Test
exp_char = testCase "exp char" $ [cexp|$char:a|] @=?  [cexp|'a'|]
    where
      a = 'a'

exp_string :: Test
exp_string =
    testCase "exp string" $ [cexp|$string:hello|] @=?  [cexp|"Hello, world\n"|]
  where
    hello = "Hello, world\n"

exp_exp :: Test
exp_exp =
    testCase "exp expression" $ [cexp|$exp:e1 + $exp:e2|] @=?  [cexp|1 + 2|]
  where
    e1 = [cexp|1|]
    e2 = [cexp|2|]

exp_func :: Test
exp_func = testCase "exp function" $
    [cunit|$func:f|]
      @=? [cunit|int add(int x) { return x + 10; }|]
  where
    f = add 10
    add n = [cfun|int add(int x) { return x + $int:n; } |]

exp_args :: Test
exp_args = testCase "exp args" $
    [cstm|f($exp:e1, $args:args, $exp:e2);|]
      @=? [cstm|f(1, 1, 2, 2);|]
  where
    e1 = [cexp|1|]
    e2 = [cexp|2|]
    args = [e1, e2]

exp_decl :: Test
exp_decl = testCase "exp decl" $
    [cfun|int inc(int n) {
             $decl:d1;
             $decls:decls

             return n + 1;
          }|]
       @=? [cfun|int inc(int n) {
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

exp_sdecl :: Test
exp_sdecl = testCase "exp sdecl" $
    [cty|struct foo { $sdecl:d1 $sdecls:decls }|]
      @=? [cty|struct foo { int i; int j; char c; }|]
  where
    d1 = [csdecl|int i;|]
    d2 = [csdecl|int j;|]
    d3 = [csdecl|char c;|]
    decls = [d2, d3]

exp_enum :: Test
exp_enum = testCase "exp enum" $
    [cty|enum foo { $enum:enum1, $enums:enums }|]
      @=? [cty|enum foo { A = 0, B, C = 2 }|]
  where
    enum1 = [cenum|A = 0|]
    enum2 = [cenum|B|]
    enum3 = [cenum|C = 2|]
    enums = [enum2, enum3]

exp_edecl :: Test
exp_edecl = testCase "exp edecl" $
    [cunit|$edecl:d1 $edecls:decls|]
      @=? [cunit|int i; int j; char c = 'c';|]
  where
    d1 = [cedecl|int i;|]
    d2 = [cedecl|int j;|]
    d3 = [cedecl|char c = 'c';|]
    decls = [d2, d3]

exp_stm :: Test
exp_stm = testCase "exp stm" $
    [cfun|int add(int x) { $stms:stms return x + 1; }|]
      @=? [cfun|int add(int x) { a = 1; b = 2; return x + 1; }|]
  where
    one = 1
    stm1 = [cstm|a = $int:one;|]
    stm2 = [cstm|b = 2;|]
    stms = [stm1, stm2]

exp_param :: Test
exp_param = testCase "exp param" $
    [cdecl|int f($param:ty1, $params:tys);|]
      @=? [cdecl|int f(char, int, float);|]
  where
    ty1 = [cparam|char|]
    ty2 = [cparam|int|]
    ty3 = [cparam|float|]
    tys = [ty2, ty3]

exp_ty :: Test
exp_ty = testCase "exp ty" $
    [cdecl|$ty:ty1 f(const $ty:ty2);|]
      @=? [cdecl|int f(const float);|]
  where
    ty1 = [cty|int|]
    ty2 = [cty|float|]

pat_args :: Test
pat_args =
    testCase"pat args" $ stms @=?  [[cexp|2|], [cexp|3|]]
  where
    stms = case [cstm|f(1, 2, 3);|] of
             [cstm|f(1, $args:es);|] -> es
             _ -> []

exp_hexp :: Test
exp_hexp =
    testCase "exp hexp" $ [cexp|$ulint:(13 - 2*5)|] @=? [cexp|3UL|]

exp_init :: Test
exp_init = testCase "initializer" $
    [cinit|{$init:initializer, .a = 10}|] @=? [cinit|{{.d = 1}, .a = 10}|]
  where
    initializer = [cinit|{.d = 1}|]

exp_inits :: Test
exp_inits = testCase "initializers" $
    [cinit|{$inits:([initializer1, initializer2])}|] @=? [cinit|{{.d = 1},{.a = 10}}|]
  where
    initializer1 = [cinit|{.d = 1}|]
    initializer2 = [cinit|{.a = 10}|]

exp_item :: Test
exp_item = testCase "exp item" $
    [cfun|int add(int x) { int y = 2; return x + y; }|]
      @=? [cfun|int add(int x) { $items:([item1, item2]) }|]
  where
    item1 = [citem|int y = 2;|]
    item2 = [citem|return x + y;|]

-------------------------------------------------------------------------------
-- OpenCL tests
-------------------------------------------------------------------------------

-- | Make sure all the OpenCL types and reserved types parse with OCL.cty
-- As of OpenCL 1.2 the types are: http://www.khronos.org/files/opencl-1-2-quick-reference-card.pdf
-- reproduced here: 
--   * Builtin Scalar types: bool, char, unsigned char, uchar, short, unsigned short, ushort,
--   int, unsigned int, uint, long, unsigned long, ulong, float, double, half, size_t, 
--   ptrdiff_t, intptr_t, uintptr_t, void
--   * Bultin Vector data types of sizes with n = 2, 3, 4, 8, or 16:
--   charn, ucharn, shortn, ushortn, intn, uintn, longn, ulongn, floatn, doublen
--   * Other builtin types:
--   image2d_t, image3d_t, image2d_array_t, image1d_t, image1d_buffer_t, image1d_array_t,
--   sampler_t, event_t 
--   * Reserved types with n,m = 2,3,4,8, or 16:
--   booln, halfn, quad, quadn, complex half, complex halfn, imaginary half, imaginary halfn,
--   complex float, complex floatn, imaginary float, imaginary floatn, complex double,
--   complex doublen, imaginary double, imaginary doublen, complex quad, complex quadn,
--   imaginary quad, imaginary quadn, floatnxm, doublenxm

ocl_ty :: Test
ocl_ty =  testCase "OpenCL Types" $ mapM_ refl
  [ -- Bultin Scalar types:
    [OCL.cty|bool|]
  , [OCL.cty|char|]
  , [OCL.cty|unsigned char|]
  , [OCL.cty|uchar|]
  , [OCL.cty|short|]
  , [OCL.cty|unsigned short|]
  , [OCL.cty|ushort|]
  , [OCL.cty|int|]
  , [OCL.cty|unsigned int|]
  , [OCL.cty|uint|]
  , [OCL.cty|long|]
  , [OCL.cty|unsigned long|]
  , [OCL.cty|ulong|]
  , [OCL.cty|float|]
  , [OCL.cty|double|]
  , [OCL.cty|half|]
  , [OCL.cty|size_t|]
  , [OCL.cty|ptrdiff_t|]
  , [OCL.cty|intdiff_t|]
  , [OCL.cty|intprt_t|]
  , [OCL.cty|uintprt_t|]
  , [OCL.cty|void|]
  ] ++
  -- Builtin vector types (and reserved vector types)
  $(let mkTypeNs ts ns = listE $ [ quoteExp OCL.cty (t ++ show n) | t <- ts, n <- ns ]
    in  mkTypeNs ["char","uchar","short","int","long","ulong","float","double"
                 ,"bool","half","quad"]
                 [2,3,4,8,16]
  ) ++
  -- Other builtin types:
  [ [OCL.cty|image2d_t|]
  , [OCL.cty|image3d_t|]
  , [OCL.cty|image2d_array_t|]
  , [OCL.cty|image1d_t|]
  , [OCL.cty|image1d_buffer_t|]
  , [OCL.cty|image1d_array_t|]
  , [OCL.cty|sampler_t|]
  , [OCL.cty|event_t|]
  ] ++
  -- matrix reserved types:
  $(let mkTypeNxMs ts ns = listE $ [ quoteExp OCL.cty (t ++ show n ++ "x" ++ show m)
                                   | t <- ts, n <- ns, m <- ns ]
    in  mkTypeNxMs ["float", "double"] [2,3,4,8,16]
  )
  -- TODO: add complex and imaginary reserved types
  where refl x = x @=? x
