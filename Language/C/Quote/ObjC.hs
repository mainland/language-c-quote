-- |
-- Module      :  Language.C.Quote.ObjC
-- Copyright   :  (c) Harvard University 2006-2011
--                (c) Geoffrey Mainland 2011-2013
--                (c) Manuel M T Chakravarty 2013
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu

{-# LANGUAGE FlexibleInstances #-}

module Language.C.Quote.ObjC (
    ToExp(..),
    toObjCLit,
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
    cfun,
    ocprop,
    ocdictelem,
    ocpropattr,
    ocmethodparam,
    ocmethodproto,
    ocmethoddef,
    ocmethodrecv,
    ocarg
  ) where

import qualified Language.C.Parser as P
import qualified Language.C.Syntax as C
import Language.C.Quote.Base (ToExp(..), quasiquote)
import Language.Haskell.TH.Quote (QuasiQuoter)

exts :: [C.Extensions]
exts = [C.ObjC]

typenames :: [String]
typenames = ["id"]

newtype ObjCLit a = ObjCLit a
    deriving (Show, Read, Eq, Ord)

instance ToExp (ObjCLit String) where
    toExp (ObjCLit s) loc = C.ObjCLitString [C.StringConst [show s] s loc] loc

instance ToExp (ObjCLit Bool) where
    toExp (ObjCLit b) loc = C.ObjCLitBool b loc

instance ToExp (ObjCLit Char) where
    toExp (ObjCLit c) loc = C.ObjCLitConst Nothing (C.CharConst (show c) c loc) loc

toObjCLit :: a -> ObjCLit a
toObjCLit = ObjCLit

cdecl, cedecl, cenum, cexp, cfun, cinit, cparam, csdecl, cstm :: QuasiQuoter
citem, cty, cunit, ocprop, ocdictelem, ocpropattr, ocmethodparam, ocmethodproto :: QuasiQuoter
ocmethoddef, ocmethodrecv, ocarg :: QuasiQuoter
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
ocprop = quasiquote exts typenames P.parsePropDecl
ocdictelem = quasiquote exts typenames P.parseDictElem
ocpropattr = quasiquote exts typenames P.parsePropAttr
ocmethodparam = quasiquote exts typenames P.parseObjcMethodArg
ocmethodproto = quasiquote exts typenames P.parseObjcMethodProto
ocmethoddef = quasiquote exts typenames P.parseObjcMethodDefn
ocmethodrecv = quasiquote exts typenames P.parseObjcMethodRecv
ocarg = quasiquote exts typenames P.parseObjcKeywordArg
