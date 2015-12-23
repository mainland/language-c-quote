-- |
-- Module      :  Language.C.Quote.CUDA
-- Copyright   :  (c) 2006-2011 Harvard University
--                (c) 2011-2013 Geoffrey Mainland
--             :  (c) 2013-2015 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu
-- The quasiquoters exposed by this module support the CUDA extensions, including CUDA-specific declaration specifiers and @\<\<\<…>>>@ kernel invocation syntax.
--
-- It includees partial support for C++11 lambda expressions syntax.
--
-- Support for lambda-expressions has the following limitations:
--
--  * the capture list must either be empty or have only the default capture mode specifier;
--
--  * the return type cannot be explicitly specified;
--
--  * the package supports C language, not C++, therefore lambda parameter list and body must be in valid C syntax.
--
-- Examples of lambdas supported by the 'cexp' quasiquoter:
--
-- > [] (int i) mutable {}
--
-- > [&] { return 7; }
--

module Language.C.Quote.CUDA (
    ToIdent(..),
    ToConst(..),
    ToExp(..),
    cexp,
    cedecl,
    cdecl,
    csdecl,
    cenum,
    ctyquals,
    cty,
    cparam,
    cparams,
    cinit,
    cstm,
    cstms,
    citem,
    citems,
    cunit,
    cfun
  ) where

import qualified Language.C.Parser as P
import qualified Language.C.Syntax as C
import Language.C.Quote.Base (ToIdent(..), ToConst(..), ToExp(..), quasiquote)
import Language.Haskell.TH.Quote (QuasiQuoter)

exts :: [C.Extensions]
exts = [C.CUDA]

typenames :: [String]
typenames =
  concatMap (typeN 4) ["char", "uchar", "short", "ushort",
                       "int",  "uint",  "long",  "ulong",
                       "longlong", "ulonglong",
                       "float", "double"] ++
  ["dim3"]

typeN :: Int -> String -> [String]
typeN k typename = [typename ++ show n | n <- [1..k]]

cdecl, cedecl, cenum, cexp, cfun, cinit, cparam, cparams, csdecl, cstm, cstms :: QuasiQuoter
citem, citems, ctyquals, cty, cunit :: QuasiQuoter
cdecl    = quasiquote exts typenames P.parseDecl
cedecl   = quasiquote exts typenames P.parseEdecl
cenum    = quasiquote exts typenames P.parseEnum
cexp     = quasiquote exts typenames P.parseExp
cfun     = quasiquote exts typenames P.parseFunc
cinit    = quasiquote exts typenames P.parseInit
cparam   = quasiquote exts typenames P.parseParam
cparams  = quasiquote exts typenames P.parseParams
csdecl   = quasiquote exts typenames P.parseStructDecl
cstm     = quasiquote exts typenames P.parseStm
cstms    = quasiquote exts typenames P.parseStms
citem    = quasiquote exts typenames P.parseBlockItem
citems   = quasiquote exts typenames P.parseBlockItems
ctyquals = quasiquote exts typenames P.parseTypeQuals
cty      = quasiquote exts typenames P.parseType
cunit    = quasiquote exts typenames P.parseUnit
