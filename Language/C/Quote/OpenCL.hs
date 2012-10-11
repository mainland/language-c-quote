-- |
-- Module      :  Language.C.Quote.OpenCL
-- Copyright   :  (c) Harvard University 2006-2011
-- License     :  BSD-style
-- Maintainer  :  Martin Dybdal <dybber@dybber.dk>

module Language.C.Quote.OpenCL (
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
exts = [C.OpenCL]


typenames :: [String]
typenames =
    concatMap typeN
    ["char", "uchar", "short", "ushort", "int", "uint",
     "long" , "ulong", "float", "double", "bool", "half", "quad"] ++
    ["uchar", "ushort", "uint", "ulong",
    "half", "quad", "image2d_t", "image3d_t", "sampler_t", "event_t"]

typeN :: String -> [String]
typeN typename = [typename ++ show n | n <- [2, 3, 4, 8, 16]]

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
