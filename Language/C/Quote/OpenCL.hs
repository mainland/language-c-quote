-- |
-- Module      :  Language.C.Quote.OpenCL
-- Copyright   :  (c) Harvard University 2006-2011
--                (c) Geoffrey Mainland 2011-2013
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu

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
    citem,
    cunit,
    cfun
  ) where

import qualified Language.C.Parser as P
import qualified Language.C.Syntax as C
import Language.C.Quote.Base (ToExp(..), quasiquote)
import Language.Haskell.TH.Quote (QuasiQuoter)

exts :: [C.Extensions]
exts = [C.OpenCL]

typenames :: [String]
typenames =
    concatMap typeN
    ["char", "uchar", "short", "ushort", "int", "uint",
     "long" , "ulong", "float", "double", "bool", "half", "quad"] ++
    ["uchar", "ushort", "uint", "ulong", "bool",
    "half", "quad", "image2d_t", "image3d_t", "sampler_t", "event_t"]

typeN :: String -> [String]
typeN typename = [typename ++ show n | n <- [2, 3, 4, 8, 16 :: Integer]]

cdecl, cedecl, cenum, cexp, cfun, cinit, cparam, csdecl, cstm :: QuasiQuoter
citem, cty, cunit :: QuasiQuoter
cdecl  = quasiquote exts typenames P.parseDecl
cedecl = quasiquote exts typenames P.parseEdecl
cenum  = quasiquote exts typenames P.parseEnum
cexp   = quasiquote exts typenames P.parseExp
cfun   = quasiquote exts typenames P.parseFunc
cinit  = quasiquote exts typenames P.parseInit
cparam = quasiquote exts typenames P.parseParam
csdecl = quasiquote exts typenames P.parseStructDecl
cstm   = quasiquote exts typenames P.parseStm
citem  = quasiquote exts typenames P.parseBlockItem
cty    = quasiquote exts typenames P.parseType
cunit  = quasiquote exts typenames P.parseUnit
