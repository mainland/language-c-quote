-- |
-- Module      :  Language.C.Quote.OpenCL
-- Copyright   :  (c) Harvard University 2006-2011
--                (c) Geoffrey Mainland 2011-2013
-- License     :  BSD-style
-- Maintainer  :  mainland@cs.drexel.edu

module Language.C.Quote.OpenCL (
    ToIdent(..),
    ToConst(..),
    ToExp(..),
    cexp,
    cedecl,
    cdecl,
    csdecl,
    cenum,
    cty,
    cparam,
    cparams,
    cinit,
    cstm,
    cstms,
    citem,
    cunit,
    cfun
  ) where

import qualified Language.C.Parser as P
import qualified Language.C.Syntax as C
import Language.C.Quote.Base (ToIdent(..), ToConst(..), ToExp(..), quasiquote)
import Language.Haskell.TH.Quote (QuasiQuoter)

exts :: [C.Extensions]
exts = [C.OpenCL]

typenames :: [String]
typenames =
    ["bool", "char", "uchar", "short", "ushort", "int", "uint",
     "long" , "ulong", "float", "half"]
    ++ ["size_t", "ptrdiff_t", "intptr_t", "uintpyt_t", "void"]
    ++ concatMap typeN ["char", "uchar", "short", "ushort",
                        "int", "uint", "long", "ulong", "float"]
    ++ ["image2d_t", "image3d_t", "sampler_t", "event_t"]
    -- OpenCL 1.2
    ++ ["double"]
    ++ concatMap typeN ["double"]
    ++ ["image2d_array_t", "image1d_t", "image1d_buffer_t", "image1d_array_t"]

typeN :: String -> [String]
typeN typename = [typename ++ show n | n <- [2, 3, 4, 8, 16 :: Integer]]

cdecl, cedecl, cenum, cexp, cfun, cinit, cparam, cparams, csdecl, cstm, cstms :: QuasiQuoter
citem, cty, cunit :: QuasiQuoter
cdecl   = quasiquote exts typenames P.parseDecl
cedecl  = quasiquote exts typenames P.parseEdecl
cenum   = quasiquote exts typenames P.parseEnum
cexp    = quasiquote exts typenames P.parseExp
cfun    = quasiquote exts typenames P.parseFunc
cinit   = quasiquote exts typenames P.parseInit
cparam  = quasiquote exts typenames P.parseParam
cparams = quasiquote exts typenames P.parseParams
csdecl  = quasiquote exts typenames P.parseStructDecl
cstm    = quasiquote exts typenames P.parseStm
cstms   = quasiquote exts typenames P.parseStms
citem   = quasiquote exts typenames P.parseBlockItem
cty     = quasiquote exts typenames P.parseType
cunit   = quasiquote exts typenames P.parseUnit
