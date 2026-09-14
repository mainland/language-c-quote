-- |
-- Module      :  Language.C.Quote.ObjC
-- Copyright   :  (c) 2006-2011 Harvard University
--                (c) 2011-2013 Geoffrey Mainland
--                (c) 2013-2014 Manuel M T Chakravarty
--             :  (c) 2013-2015 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

{-# LANGUAGE FlexibleInstances #-}

module Language.C.Quote.ObjC (
    ToIdent(..),
    ToConst(..),
    ToExp(..),
    objcLit,
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
    cfun,
    objcprop,
    objcifdecls,
    objcimdecls,
    objcdictelem,
    objcpropattr,
    objcmethparam,
    objcmethproto,
    objcmethdef,
    objcmethrecv,
    objcarg
  ) where

import qualified Language.C.Parser as P
import qualified Language.C.Syntax as C
import Language.C.Quote.Base (ToIdent(..), ToConst(..), ToExp(..), quasiquote)
import Language.Haskell.TH.Quote (QuasiQuoter)

exts :: [C.Extensions]
exts = [C.ObjC, C.Blocks, C.Gcc]

typenames :: [String]
typenames = ["id", "instancetype"]

-- | A wrapper for a value indicating that it should be treated as an
-- Objective-C literal.
newtype ObjCLit a = ObjCLit a
    deriving (Show, Read, Eq, Ord)

instance ToExp (ObjCLit String) where
    toExp (ObjCLit s) loc = C.ObjCLitString [C.StringConst [show s] s loc] loc

instance ToExp (ObjCLit Bool) where
    toExp (ObjCLit b) loc = C.ObjCLitBool b loc

instance ToExp (ObjCLit Char) where
    toExp (ObjCLit c) loc = C.ObjCLitConst Nothing (C.CharConst (show c) c loc) loc

-- | Indicates that a value should be treated as an Objective-C literal.
objcLit :: a -> ObjCLit a
objcLit = ObjCLit

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

objcprop, objcpropattr, objcifdecls, objcimdecls, objcdictelem, objcmethparam, objcmethproto :: QuasiQuoter
objcmethdef, objcmethrecv, objcarg :: QuasiQuoter
objcprop      = quasiquote exts typenames P.parseObjCProp
objcifdecls   = quasiquote exts typenames P.parseObjCIfaceDecls
objcimdecls   = quasiquote exts typenames P.parseObjCImplDecls
objcpropattr  = quasiquote exts typenames P.parseObjCPropAttr
objcdictelem  = quasiquote exts typenames P.parseObjCDictElem
objcmethparam = quasiquote exts typenames P.parseObjCMethodParam
objcmethproto = quasiquote exts typenames P.parseObjCMethodProto
objcmethdef   = quasiquote exts typenames P.parseObjCMethodDef
objcmethrecv  = quasiquote exts typenames P.parseObjCMethodRecv
objcarg       = quasiquote exts typenames P.parseObjCKeywordArg
