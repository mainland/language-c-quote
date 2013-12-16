-- |
-- Module      :  Language.C.Quote.ObjC
-- Copyright   :  (c) Harvard University 2006-2011
--                (c) Geoffrey Mainland 2011-2013
--                (c) Manuel M T Chakravarty 2013
--             :  (c) Drexel University 2013
-- License     :  BSD-style
-- Maintainer  :  mainland@cs.drexel.edu

module Language.C.Quote.ObjC (
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
exts = [C.ObjC]

typenames :: [String]
typenames = ["id"]

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
