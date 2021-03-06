{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Data.Generics

import Language.C.Syntax

import Derive

main :: IO ()
main = do
#undef DERIVE
#define DERIVE(a) deriveM deriveLocated (undefined::a)
    DERIVE(Id)
    DERIVE(StringLit)
    DERIVE(Storage)
    DERIVE(TypeQual)
    DERIVE(Sign)
    DERIVE(TypeSpec)
    DERIVE(DeclSpec)
    DERIVE(ArraySize)
    DERIVE(Decl)
    DERIVE(Type)
    DERIVE(Designator)
    DERIVE(Designation)
    DERIVE(Initializer)
    DERIVE(Init)
    DERIVE(Typedef)
    DERIVE(InitGroup)
    DERIVE(Field)
    DERIVE(FieldGroup)
    DERIVE(CEnum)
    DERIVE(Attr)
    DERIVE(Param)
    DERIVE(Params)
    DERIVE(Func)
    DERIVE(Definition)
    DERIVE(Stm)
    DERIVE(BlockItem)
    DERIVE(Const)
    DERIVE(Exp)
    DERIVE(LambdaIntroducer)
    DERIVE(LambdaDeclarator)
    DERIVE(BlockType)
    DERIVE(ExeConfig)
    DERIVE(ObjCIvarDecl)
    DERIVE(ObjCVisibilitySpec)
    DERIVE(ObjCIfaceDecl)
    DERIVE(ObjCPropAttr)
    DERIVE(ObjCMethodReq)
    DERIVE(ObjCParam)
    DERIVE(ObjCMethodProto)
    DERIVE(ObjCCatch)
    DERIVE(ObjCRecv)
    DERIVE(ObjCArg)
    DERIVE(ObjCDictElem)

#undef DERIVE
#define DERIVE(a) deriveM deriveRelocatable (undefined::a)
    DERIVE(Id)
    DERIVE(StringLit)
    DERIVE(Storage)
    DERIVE(TypeQual)
    DERIVE(Sign)
    DERIVE(TypeSpec)
    DERIVE(DeclSpec)
    DERIVE(ArraySize)
    DERIVE(Decl)
    DERIVE(Type)
    DERIVE(Designator)
    DERIVE(Designation)
    DERIVE(Initializer)
    DERIVE(Init)
    DERIVE(Typedef)
    DERIVE(InitGroup)
    DERIVE(Field)
    DERIVE(FieldGroup)
    DERIVE(CEnum)
    DERIVE(Attr)
    DERIVE(Param)
    DERIVE(Params)
    DERIVE(Func)
    DERIVE(Definition)
    DERIVE(Stm)
    DERIVE(BlockItem)
    DERIVE(Const)
    DERIVE(Exp)
    DERIVE(LambdaIntroducer)
    DERIVE(LambdaDeclarator)
    DERIVE(BlockType)
    DERIVE(ExeConfig)
    DERIVE(ObjCIvarDecl)
    DERIVE(ObjCVisibilitySpec)
    DERIVE(ObjCIfaceDecl)
    DERIVE(ObjCPropAttr)
    DERIVE(ObjCMethodReq)
    DERIVE(ObjCParam)
    DERIVE(ObjCMethodProto)
    DERIVE(ObjCCatch)
    DERIVE(ObjCRecv)
    DERIVE(ObjCArg)
    DERIVE(ObjCDictElem)
