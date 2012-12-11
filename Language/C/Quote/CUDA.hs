-- |
-- Module      :  Language.C.Quote.CUDA
-- Copyright   :  (c) Harvard University 2006-2011
--                (c) Geoffrey Mainland 2011-2012
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu

module Language.C.Quote.CUDA (
    ToExp(..),
    cexp,
    cedecl,
    cdecl,
    csdecl,
    cenum,
    cty,
    cparam,
    cinit,
    cstm,
    cunit,
    cfun
  ) where

import qualified Language.C.Parser as P
import qualified Language.C.Syntax as C
import Language.C.Quote.Base (ToExp(..), quasiquote)

exts :: [C.Extensions]
exts = [C.CUDA]

typenames :: [String]
typenames =
  concatMap (typeN 4) ["char", "uchar", "short", "ushort",
                       "int",  "uint",  "long",  "ulong",
                       "float", "double"] ++
  typeN 2 "longlong" ++
  ["dim3"]

typeN :: Int -> String -> [String]
typeN k typename = [typename ++ show n | n <- [1..k]]

cdecl  = quasiquote exts typenames P.parseDecl
cedecl = quasiquote exts typenames P.parseEdecl
cenum  = quasiquote exts typenames P.parseEnum
cexp   = quasiquote exts typenames P.parseExp
cfun   = quasiquote exts typenames P.parseFunc
cinit  = quasiquote exts typenames P.parseInit
cparam = quasiquote exts typenames P.parseParam
csdecl = quasiquote exts typenames P.parseStructDecl
cstm   = quasiquote exts typenames P.parseStm
cty    = quasiquote exts typenames P.parseType
cunit  = quasiquote exts typenames P.parseUnit
