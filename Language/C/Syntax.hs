-- Copyright (c) 2006-2010
--         The President and Fellows of Harvard College.
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
-- 3. Neither the name of the University nor the names of its contributors
--    may be used to endorse or promote products derived from this software
--    without specific prior written permission.

-- THIS SOFTWARE IS PROVIDED BY THE UNIVERSITY AND CONTRIBUTORS ``AS IS'' AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED.  IN NO EVENT SHALL THE UNIVERSITY OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
-- LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
-- OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.

--------------------------------------------------------------------------------
-- |
-- Module      :  Language.C.Syntax
-- Copyright   :  (c) Harvard University 2006-2010
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.C.Syntax where

import Data.Generics

import Data.Loc

data Extensions = Gcc
                | CUDA
  deriving (Eq, Ord, Enum, Show)

data Id = Id String !SrcLoc
        | AntiId String !SrcLoc
    deriving (Eq, Ord, Data, Typeable)

data Storage = Tauto !SrcLoc
             | Tregister !SrcLoc
             | Tstatic !SrcLoc
             | Textern !SrcLoc
             | TexternL String !SrcLoc
             | Ttypedef !SrcLoc
    deriving (Eq, Ord, Data, Typeable)

data TypeQual = Tconst !SrcLoc
              | Tvolatile !SrcLoc
              | Tinline !SrcLoc

              -- C99
              | Trestrict !SrcLoc

              -- CUDA
              | Tdevice !SrcLoc
              | Tglobal !SrcLoc
              | Thost !SrcLoc
              | Tconstant !SrcLoc
              | Tshared !SrcLoc
              | Tnoinline !SrcLoc
    deriving (Eq, Ord, Data, Typeable)

data Sign = Tsigned !SrcLoc
          | Tunsigned !SrcLoc
    deriving (Eq, Ord, Data, Typeable)

data TypeSpec = Tvoid !SrcLoc
              | Tchar (Maybe Sign) !SrcLoc
              | Tshort (Maybe Sign) !SrcLoc
              | Tint (Maybe Sign) !SrcLoc
              | Tlong (Maybe Sign) !SrcLoc
              | Tlong_long (Maybe Sign) !SrcLoc
              | Tfloat !SrcLoc
              | Tdouble !SrcLoc
              | Tlong_double !SrcLoc
              | Tstruct (Maybe Id) (Maybe [FieldGroup]) [Attr] !SrcLoc
              | Tunion (Maybe Id) (Maybe [FieldGroup]) [Attr] !SrcLoc
              | Tenum (Maybe Id) [CEnum] [Attr] !SrcLoc
              | Tnamed Id !SrcLoc
              | TtypeofExp Exp !SrcLoc
              | TtypeofType Type !SrcLoc
              | Tva_list !SrcLoc
    deriving (Eq, Ord, Data, Typeable)

data DeclSpec = DeclSpec [Storage] [TypeQual] TypeSpec !SrcLoc
              | AntiDeclSpec String !SrcLoc
              | AntiTypeDeclSpec [Storage] [TypeQual] String !SrcLoc
    deriving (Eq, Ord, Data, Typeable)

-- | There are two types of declarators in C, regular declarators and abstract
-- declarators. The former is for declaring variables, function parameters,
-- typedefs, etc. and the latter for abstract types---@typedef int
-- ({*}foo)(void)@ vs. @\tt int ({*})(void)@. The difference between the two is
-- just whether or not an identifier is attached to the declarator. We therefore
-- only define one 'Decl' type and use it for both cases.

data ArraySize = ArraySize Bool Exp !SrcLoc
               | VariableArraySize !SrcLoc
               | NoArraySize !SrcLoc
    deriving (Eq, Ord, Data, Typeable)

data Decl = DeclRoot !SrcLoc
          | Ptr [TypeQual] Decl !SrcLoc
          | Array [TypeQual] ArraySize Decl !SrcLoc
          | Proto Decl Params !SrcLoc
          | OldProto Decl [Id] !SrcLoc
          | AntiTypeDecl String !SrcLoc
    deriving (Eq, Ord, Data, Typeable)

data Type = Type DeclSpec Decl !SrcLoc
          | AntiType String !SrcLoc
    deriving (Eq, Ord, Data, Typeable)

data Designator = IndexDesignator Exp !SrcLoc
                | MemberDesignator Id !SrcLoc
    deriving (Eq, Ord, Data, Typeable)

data Designation = Designation [Designator] !SrcLoc
    deriving (Eq, Ord, Data, Typeable)

data Initializer = ExpInitializer Exp !SrcLoc
                 | CompoundInitializer [(Maybe Designation, Initializer)] !SrcLoc
    deriving (Eq, Ord, Data, Typeable)

type AsmLabel = String

data Init = Init Id Decl (Maybe AsmLabel) (Maybe Initializer) [Attr] !SrcLoc
    deriving (Eq, Ord, Data, Typeable)

data Typedef = Typedef Id Decl [Attr] !SrcLoc
    deriving (Eq, Ord, Data, Typeable)

data InitGroup = InitGroup DeclSpec [Attr] [Init] !SrcLoc
               | TypedefGroup DeclSpec [Attr] [Typedef] !SrcLoc
               | AntiDecl String !SrcLoc
               | AntiDecls String !SrcLoc
    deriving (Eq, Ord, Data, Typeable)

data Field = Field (Maybe Id) (Maybe Decl) (Maybe Exp) !SrcLoc
    deriving (Eq, Ord, Data, Typeable)

data FieldGroup  =  FieldGroup DeclSpec [Field] !SrcLoc
                 |  AntiSdecl String !SrcLoc
                 |  AntiSdecls String !SrcLoc
    deriving (Eq, Ord, Data, Typeable)

data CEnum  =  CEnum Id (Maybe Exp) !SrcLoc
            |  AntiEnum String !SrcLoc
            |  AntiEnums String !SrcLoc
    deriving (Eq, Ord, Data, Typeable)

data Attr  =  Attr Id [Exp] !SrcLoc
    deriving (Eq, Ord, Data, Typeable)

data Param  =  Param (Maybe Id) DeclSpec Decl !SrcLoc
            |  AntiParam String !SrcLoc
            |  AntiParams String !SrcLoc
    deriving (Eq, Ord, Data, Typeable)

data Params = Params [Param] Bool !SrcLoc
    deriving (Eq, Ord, Data, Typeable)

data Func  =  Func DeclSpec Id Decl Params [BlockItem] !SrcLoc
           |  OldFunc DeclSpec Id Decl [Id] (Maybe [InitGroup]) [BlockItem] !SrcLoc
    deriving (Eq, Ord, Data, Typeable)

data Definition  =  FuncDef Func !SrcLoc
                 |  DecDef InitGroup !SrcLoc
                 |  EscDef String !SrcLoc
                 |  AntiFunc String !SrcLoc
                 |  AntiEsc String !SrcLoc
                 |  AntiEdecl String !SrcLoc
                 |  AntiEdecls String !SrcLoc
    deriving (Eq, Ord, Data, Typeable)

data Stm  = Label Id Stm !SrcLoc
          | Case Exp Stm !SrcLoc
          | Default Stm !SrcLoc
          | Exp (Maybe Exp) !SrcLoc
          | Block [BlockItem] !SrcLoc
          | If Exp Stm (Maybe Stm) !SrcLoc
          | Switch Exp Stm !SrcLoc
          | While Exp Stm !SrcLoc
          | DoWhile Stm Exp !SrcLoc
          | For  (Either InitGroup (Maybe Exp)) (Maybe Exp) (Maybe Exp) Stm
                 !SrcLoc
          | Goto Id !SrcLoc
          | Continue !SrcLoc
          | Break !SrcLoc
          | Return (Maybe Exp) !SrcLoc
          | Asm Bool [Attr] [String] [(String, Exp)] [(String, Exp)] [String] !SrcLoc
          | AntiStm String !SrcLoc
          | AntiStms String !SrcLoc
    deriving (Eq, Ord, Data, Typeable)

data BlockItem = BlockDecl InitGroup
               | BlockStm Stm
               | AntiBlockItem String !SrcLoc
               | AntiBlockItems String !SrcLoc
    deriving (Eq, Ord, Data, Typeable)

funcProto :: Func -> InitGroup
funcProto f@(Func decl_spec id decl params _ _) =
    InitGroup decl_spec [] [Init id (Proto decl params loc) Nothing Nothing [] loc] loc
  where
    loc = locOf f

funcProto f@(OldFunc decl_spec id decl params _ _ _) =
    InitGroup decl_spec [] [Init id (OldProto decl params loc) Nothing Nothing [] loc]
              loc
  where
    loc = locOf f

isPtr :: Type -> Bool
isPtr  (Type _ decl _)  = go decl
  where
    go  (DeclRoot _)        = False
    go  (Ptr _ _ _)         = True
    go  (Array _ _ _ _)     = True
    go  (Proto _ _ _)       = False
    go  (OldProto _ _ _)    = False
    go  (AntiTypeDecl _ _)  = error "isPtr: encountered antiquoted type declaration"
isPtr  (AntiType _ _)       = error "isPtr: encountered antiquoted type"

data Signed = Signed
            | Unsigned
    deriving (Eq, Ord, Data, Typeable)

data Const = IntConst String Signed Integer !SrcLoc
           | LongIntConst String Signed Integer !SrcLoc
           | LongLongIntConst String Signed Integer !SrcLoc
           | FloatConst String Rational !SrcLoc
           | DoubleConst String Rational !SrcLoc
           | LongDoubleConst String Rational !SrcLoc
           | CharConst String Char !SrcLoc
           | StringConst [String] String !SrcLoc
           | AntiInt String !SrcLoc
           | AntiUInt String !SrcLoc
           | AntiLInt String !SrcLoc
           | AntiULInt String !SrcLoc
           | AntiFloat String !SrcLoc
           | AntiDouble String !SrcLoc
           | AntiLongDouble String !SrcLoc
           | AntiChar String !SrcLoc
           | AntiString String !SrcLoc
    deriving (Eq, Ord, Data, Typeable)

data ExeConfig = ExeConfig
    {  exeGridDim    :: Exp
    ,  exeBlockDim   :: Exp
    ,  exeSharedSize :: Maybe Exp
    ,  exeStream     :: Maybe Exp
    ,  exeLoc        :: !SrcLoc
    }
    deriving (Eq, Ord, Data, Typeable)

data Exp = Var Id !SrcLoc
         | Const Const !SrcLoc
         | BinOp BinOp Exp Exp !SrcLoc
         | Assign Exp AssignOp Exp !SrcLoc
         | PreInc Exp !SrcLoc
         | PostInc Exp !SrcLoc
         | PreDec Exp !SrcLoc
         | PostDec Exp !SrcLoc
         | UnOp UnOp Exp !SrcLoc
         | SizeofExp Exp !SrcLoc
         | SizeofType Type !SrcLoc
         | Cast Type Exp !SrcLoc
         | Cond Exp Exp Exp !SrcLoc
         | Member Exp Id !SrcLoc
         | PtrMember Exp Id !SrcLoc
         | Index Exp Exp !SrcLoc
         | FnCall Exp [Exp] !SrcLoc
         | CudaCall Exp ExeConfig [Exp] !SrcLoc
         | Seq Exp Exp !SrcLoc
         | CompoundLit Type [(Maybe Designation, Initializer)] !SrcLoc
         | StmExpr [BlockItem] !SrcLoc
         | BuiltinVaArg Exp Type !SrcLoc
         | AntiExp String !SrcLoc
         | AntiArgs String !SrcLoc
    deriving (Eq, Ord, Data, Typeable)

data BinOp = Add
           | Sub
           | Mul
           | Div
           | Mod
           | Eq
           | Ne
           | Lt
           | Gt
           | Le
           | Ge
           | Land
           | Lor
           | And
           | Or
           | Xor
           | Lsh
           | Rsh
    deriving (Eq, Ord, Data, Typeable)

data AssignOp = JustAssign
              | AddAssign
              | SubAssign
              | MulAssign
              | DivAssign
              | ModAssign
              | LshAssign
              | RshAssign
              | AndAssign
              | XorAssign
              | OrAssign
    deriving (Eq, Ord, Data, Typeable)

data UnOp = AddrOf
          | Deref
          | Positive
          | Negate
          | Not
          | Lnot
    deriving (Eq, Ord, Data, Typeable)

instance Located Id where
    getLoc (Id _ loc)      = getLoc loc
    getLoc (AntiId _ loc)  = getLoc loc

instance Located Storage where
    getLoc (Tauto loc)      = getLoc loc
    getLoc (Tregister loc)  = getLoc loc
    getLoc (Tstatic loc)    = getLoc loc
    getLoc (Textern loc)    = getLoc loc
    getLoc (TexternL _ loc) = getLoc loc
    getLoc (Ttypedef loc)   = getLoc loc

instance Located TypeQual where
    getLoc (Tconst loc)     = getLoc loc
    getLoc (Tvolatile loc)  = getLoc loc
    getLoc (Tinline loc)    = getLoc loc

    getLoc (Trestrict loc)  = getLoc loc

    getLoc (Tdevice loc)    = getLoc loc
    getLoc (Tglobal loc)    = getLoc loc
    getLoc (Thost loc)      = getLoc loc
    getLoc (Tconstant loc)  = getLoc loc
    getLoc (Tshared loc)    = getLoc loc
    getLoc (Tnoinline loc)  = getLoc loc

instance Located Sign where
    getLoc (Tsigned loc)    = getLoc loc
    getLoc (Tunsigned loc)  = getLoc loc

instance Located TypeSpec where
    getLoc (Tvoid loc)          = getLoc loc
    getLoc (Tchar _ loc)        = getLoc loc
    getLoc (Tshort _ loc)       = getLoc loc
    getLoc (Tint _ loc)         = getLoc loc
    getLoc (Tlong _ loc)        = getLoc loc
    getLoc (Tlong_long _ loc)   = getLoc loc
    getLoc (Tfloat loc)         = getLoc loc
    getLoc (Tdouble loc)        = getLoc loc
    getLoc (Tlong_double loc)   = getLoc loc
    getLoc (Tstruct _ _ _ loc)  = getLoc loc
    getLoc (Tunion _ _ _ loc)   = getLoc loc
    getLoc (Tenum _ _ _ loc)    = getLoc loc
    getLoc (Tnamed _ loc)       = getLoc loc
    getLoc (TtypeofExp _ loc)   = getLoc loc
    getLoc (TtypeofType _ loc)  = getLoc loc
    getLoc (Tva_list loc)       = getLoc loc

instance Located DeclSpec where
    getLoc (DeclSpec _ _ _ loc)          = getLoc loc
    getLoc (AntiDeclSpec _ loc)          = getLoc loc
    getLoc (AntiTypeDeclSpec _ _ _ loc)  = getLoc loc

instance Located ArraySize where
    getLoc (ArraySize _ _ loc)     = getLoc loc
    getLoc (VariableArraySize loc) = getLoc loc
    getLoc (NoArraySize loc)       = getLoc loc

instance Located Decl where
    getLoc (DeclRoot loc)        = getLoc loc
    getLoc (Ptr _ _ loc)         = getLoc loc
    getLoc (Array _ _ _ loc)     = getLoc loc
    getLoc (Proto _ _ loc)       = getLoc loc
    getLoc (OldProto _ _ loc)    = getLoc loc
    getLoc (AntiTypeDecl _ loc)  = getLoc loc

instance Located Type where
    getLoc (Type _ _ loc)    = getLoc loc
    getLoc (AntiType _ loc)  = getLoc loc

instance Located Designator where
    getLoc (IndexDesignator _ loc)   = getLoc loc
    getLoc (MemberDesignator _ loc)  = getLoc loc

instance Located Designation where
    getLoc (Designation _ loc)   = getLoc loc

instance Located Initializer where
    getLoc (ExpInitializer _ loc)       = getLoc loc
    getLoc (CompoundInitializer _ loc)  = getLoc loc

instance Located Init where
    getLoc (Init _ _ _ _ _ loc) = getLoc loc

instance Located Typedef where
    getLoc (Typedef _ _ _ loc) = getLoc loc

instance Located InitGroup where
    getLoc (InitGroup _ _ _ loc)     = getLoc loc
    getLoc (TypedefGroup _ _ _ loc)  = getLoc loc
    getLoc (AntiDecl _ loc)          = getLoc loc
    getLoc (AntiDecls _ loc)         = getLoc loc

instance Located Field where
    getLoc (Field _ _ _ loc) = getLoc loc

instance Located FieldGroup where
    getLoc (FieldGroup _ _ loc)  = getLoc loc
    getLoc (AntiSdecl _ loc)     = getLoc loc
    getLoc (AntiSdecls _ loc)    = getLoc loc

instance Located CEnum where
    getLoc (CEnum _ _ loc)    = getLoc loc
    getLoc (AntiEnum _ loc)   = getLoc loc
    getLoc (AntiEnums _ loc)  = getLoc loc

instance Located Attr where
    getLoc (Attr _ _ loc) = getLoc loc

instance Located Param where
    getLoc (Param _ _ _ loc)   = getLoc loc
    getLoc (AntiParam _ loc)   = getLoc loc
    getLoc (AntiParams _ loc)  = getLoc loc

instance Located Params where
    getLoc (Params _ _ loc) = getLoc loc

instance Located Func where
    getLoc (Func _ _ _ _ _ loc)      = getLoc loc
    getLoc (OldFunc _ _ _ _ _ _ loc) = getLoc loc

instance Located Definition where
    getLoc (FuncDef _ loc)     = getLoc loc
    getLoc (DecDef _ loc)      = getLoc loc
    getLoc (EscDef _ loc)      = getLoc loc
    getLoc (AntiFunc _ loc)    = getLoc loc
    getLoc (AntiEsc _ loc)     = getLoc loc
    getLoc (AntiEdecl _ loc)   = getLoc loc
    getLoc (AntiEdecls _ loc)  = getLoc loc

instance Located Stm where
    getLoc (Label _ _ loc)       = getLoc loc
    getLoc (Case _ _ loc)        = getLoc loc
    getLoc (Default _ loc)       = getLoc loc
    getLoc (Exp _ loc)           = getLoc loc
    getLoc (Block _ loc)         = getLoc loc
    getLoc (If _ _ _ loc)        = getLoc loc
    getLoc (Switch _ _ loc)      = getLoc loc
    getLoc (While _ _ loc)       = getLoc loc
    getLoc (DoWhile _ _ loc)     = getLoc loc
    getLoc (For _ _ _ _ loc)     = getLoc loc
    getLoc (Goto _ loc)          = getLoc loc
    getLoc (Continue loc)        = getLoc loc
    getLoc (Break loc)           = getLoc loc
    getLoc (Return _ loc)        = getLoc loc
    getLoc (Asm _ _ _ _ _ _ loc) = getLoc loc
    getLoc (AntiStm _ loc)       = getLoc loc
    getLoc (AntiStms _ loc)      = getLoc loc

instance Located BlockItem where
    getLoc (BlockDecl decl)       = getLoc decl
    getLoc (BlockStm stm)         = getLoc stm
    getLoc (AntiBlockItem _ loc)  = getLoc loc
    getLoc (AntiBlockItems _ loc) = getLoc loc

instance Located Const where
    getLoc (IntConst _ _ _ loc)          = getLoc loc
    getLoc (LongIntConst _ _ _ loc)      = getLoc loc
    getLoc (LongLongIntConst _ _ _ loc)  = getLoc loc
    getLoc (FloatConst _ _ loc)          = getLoc loc
    getLoc (DoubleConst _ _ loc)         = getLoc loc
    getLoc (LongDoubleConst _ _ loc)     = getLoc loc
    getLoc (CharConst _ _ loc)           = getLoc loc
    getLoc (StringConst _ _ loc)         = getLoc loc
    getLoc (AntiInt _ loc)               = getLoc loc
    getLoc (AntiUInt _ loc)              = getLoc loc
    getLoc (AntiLInt _ loc)              = getLoc loc
    getLoc (AntiULInt _ loc)             = getLoc loc
    getLoc (AntiFloat _ loc)             = getLoc loc
    getLoc (AntiDouble _ loc)            = getLoc loc
    getLoc (AntiLongDouble _ loc)        = getLoc loc
    getLoc (AntiChar _ loc)              = getLoc loc
    getLoc (AntiString _ loc)            = getLoc loc

instance Located ExeConfig where
    getLoc conf = getLoc (exeLoc conf)

instance Located Exp where
    getLoc (Var _ loc)             = getLoc loc
    getLoc (Const _ loc)           = getLoc loc
    getLoc (BinOp _ _ _ loc)       = getLoc loc
    getLoc (Assign _ _ _ loc)      = getLoc loc
    getLoc (PreInc _ loc)          = getLoc loc
    getLoc (PostInc _ loc)         = getLoc loc
    getLoc (PreDec _ loc)          = getLoc loc
    getLoc (PostDec _ loc)         = getLoc loc
    getLoc (UnOp _ _ loc)          = getLoc loc
    getLoc (SizeofExp _ loc)       = getLoc loc
    getLoc (SizeofType _ loc)      = getLoc loc
    getLoc (Cast _ _ loc)          = getLoc loc
    getLoc (Cond _ _ _ loc)        = getLoc loc
    getLoc (Member _ _ loc)        = getLoc loc
    getLoc (PtrMember _ _ loc)     = getLoc loc
    getLoc (Index _ _ loc)         = getLoc loc
    getLoc (FnCall _ _ loc)        = getLoc loc
    getLoc (CudaCall _ _ _ loc)    = getLoc loc
    getLoc (Seq _ _ loc)           = getLoc loc
    getLoc (CompoundLit _ _ loc)   = getLoc loc
    getLoc (StmExpr _ loc)         = getLoc loc
    getLoc (BuiltinVaArg _ _ loc)  = getLoc loc
    getLoc (AntiExp _ loc)         = getLoc loc
    getLoc (AntiArgs _ loc)        = getLoc loc

ctypedef :: Id -> Decl -> [Attr] -> Typedef
ctypedef id decl attrs =
    Typedef id decl attrs ((id <--> decl :: Loc) <--> attrs)

cdeclSpec :: [Storage] -> [TypeQual] -> TypeSpec -> DeclSpec
cdeclSpec storage quals spec =
    DeclSpec storage quals spec ((storage <--> quals :: Loc) <--> spec)

cinitGroup :: DeclSpec -> [Attr] -> [Init] -> InitGroup
cinitGroup dspec attrs inis =
    InitGroup dspec attrs inis ((dspec <--> attrs :: Loc) <--> inis)

ctypedefGroup :: DeclSpec -> [Attr] -> [Typedef] -> InitGroup
ctypedefGroup dspec attrs typedefs =
    TypedefGroup dspec attrs typedefs ((dspec <--> attrs :: Loc) <--> typedefs)
