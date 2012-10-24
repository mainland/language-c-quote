-- |
-- Module      :  Language.C.Syntax
-- Copyright   :  (c) Harvard University 2006-2011
--                (c) Geoffrey Mainland 2011-2012
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.C.Syntax where

import Data.Generics

import Data.Loc

data Extensions = Antiquotation
                | Gcc
                | CUDA
                | OpenCL
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
              | TCUDAdevice !SrcLoc
              | TCUDAglobal !SrcLoc
              | TCUDAhost !SrcLoc
              | TCUDAconstant !SrcLoc
              | TCUDAshared !SrcLoc
              | TCUDArestrict !SrcLoc
              | TCUDAnoinline !SrcLoc

              -- OpenCL
              | TCLprivate !SrcLoc
              | TCLlocal !SrcLoc
              | TCLglobal !SrcLoc
              | TCLconstant !SrcLoc
              | TCLreadonly !SrcLoc
              | TCLwriteonly !SrcLoc
              | TCLkernel !SrcLoc
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
          | Pragma String !SrcLoc
          | Asm Bool [Attr] [String]
                     [(String, Exp)] [(String, Exp)]
                     [String] !SrcLoc
          | AntiPragma String !SrcLoc
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
    InitGroup decl_spec []
      [Init id (Proto decl params l) Nothing Nothing [] l] l
  where
    l = srclocOf f

funcProto f@(OldFunc decl_spec id decl params _ _ _) =
    InitGroup decl_spec []
      [Init id (OldProto decl params l) Nothing Nothing [] l] l
  where
    l = srclocOf f

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
           | AntiLLInt String !SrcLoc
           | AntiULLInt String !SrcLoc
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
    locOf (Id _ loc)      = locOf loc
    locOf (AntiId _ loc)  = locOf loc

instance Located Storage where
    locOf (Tauto loc)      = locOf loc
    locOf (Tregister loc)  = locOf loc
    locOf (Tstatic loc)    = locOf loc
    locOf (Textern loc)    = locOf loc
    locOf (TexternL _ loc) = locOf loc
    locOf (Ttypedef loc)   = locOf loc

instance Located TypeQual where
    locOf (Tconst loc)     = locOf loc
    locOf (Tvolatile loc)  = locOf loc
    locOf (Tinline loc)    = locOf loc

    locOf (Trestrict loc)  = locOf loc

    locOf (TCUDAdevice loc)    = locOf loc
    locOf (TCUDAglobal loc)    = locOf loc
    locOf (TCUDAhost loc)      = locOf loc
    locOf (TCUDAconstant loc)  = locOf loc
    locOf (TCUDAshared loc)    = locOf loc
    locOf (TCUDArestrict loc)  = locOf loc
    locOf (TCUDAnoinline loc)  = locOf loc

    locOf (TCLprivate loc)   = locOf loc
    locOf (TCLlocal loc)     = locOf loc
    locOf (TCLglobal loc)    = locOf loc
    locOf (TCLconstant loc)  = locOf loc
    locOf (TCLreadonly loc)  = locOf loc
    locOf (TCLwriteonly loc) = locOf loc
    locOf (TCLkernel loc)    = locOf loc

instance Located Sign where
    locOf (Tsigned loc)    = locOf loc
    locOf (Tunsigned loc)  = locOf loc

instance Located TypeSpec where
    locOf (Tvoid loc)          = locOf loc
    locOf (Tchar _ loc)        = locOf loc
    locOf (Tshort _ loc)       = locOf loc
    locOf (Tint _ loc)         = locOf loc
    locOf (Tlong _ loc)        = locOf loc
    locOf (Tlong_long _ loc)   = locOf loc
    locOf (Tfloat loc)         = locOf loc
    locOf (Tdouble loc)        = locOf loc
    locOf (Tlong_double loc)   = locOf loc
    locOf (Tstruct _ _ _ loc)  = locOf loc
    locOf (Tunion _ _ _ loc)   = locOf loc
    locOf (Tenum _ _ _ loc)    = locOf loc
    locOf (Tnamed _ loc)       = locOf loc
    locOf (TtypeofExp _ loc)   = locOf loc
    locOf (TtypeofType _ loc)  = locOf loc
    locOf (Tva_list loc)       = locOf loc

instance Located DeclSpec where
    locOf (DeclSpec _ _ _ loc)          = locOf loc
    locOf (AntiDeclSpec _ loc)          = locOf loc
    locOf (AntiTypeDeclSpec _ _ _ loc)  = locOf loc

instance Located ArraySize where
    locOf (ArraySize _ _ loc)     = locOf loc
    locOf (VariableArraySize loc) = locOf loc
    locOf (NoArraySize loc)       = locOf loc

instance Located Decl where
    locOf (DeclRoot loc)        = locOf loc
    locOf (Ptr _ _ loc)         = locOf loc
    locOf (Array _ _ _ loc)     = locOf loc
    locOf (Proto _ _ loc)       = locOf loc
    locOf (OldProto _ _ loc)    = locOf loc
    locOf (AntiTypeDecl _ loc)  = locOf loc

instance Located Type where
    locOf (Type _ _ loc)    = locOf loc
    locOf (AntiType _ loc)  = locOf loc

instance Located Designator where
    locOf (IndexDesignator _ loc)   = locOf loc
    locOf (MemberDesignator _ loc)  = locOf loc

instance Located Designation where
    locOf (Designation _ loc)   = locOf loc

instance Located Initializer where
    locOf (ExpInitializer _ loc)       = locOf loc
    locOf (CompoundInitializer _ loc)  = locOf loc

instance Located Init where
    locOf (Init _ _ _ _ _ loc) = locOf loc

instance Located Typedef where
    locOf (Typedef _ _ _ loc) = locOf loc

instance Located InitGroup where
    locOf (InitGroup _ _ _ loc)     = locOf loc
    locOf (TypedefGroup _ _ _ loc)  = locOf loc
    locOf (AntiDecl _ loc)          = locOf loc
    locOf (AntiDecls _ loc)         = locOf loc

instance Located Field where
    locOf (Field _ _ _ loc) = locOf loc

instance Located FieldGroup where
    locOf (FieldGroup _ _ loc)  = locOf loc
    locOf (AntiSdecl _ loc)     = locOf loc
    locOf (AntiSdecls _ loc)    = locOf loc

instance Located CEnum where
    locOf (CEnum _ _ loc)    = locOf loc
    locOf (AntiEnum _ loc)   = locOf loc
    locOf (AntiEnums _ loc)  = locOf loc

instance Located Attr where
    locOf (Attr _ _ loc) = locOf loc

instance Located Param where
    locOf (Param _ _ _ loc)   = locOf loc
    locOf (AntiParam _ loc)   = locOf loc
    locOf (AntiParams _ loc)  = locOf loc

instance Located Params where
    locOf (Params _ _ loc) = locOf loc

instance Located Func where
    locOf (Func _ _ _ _ _ loc)      = locOf loc
    locOf (OldFunc _ _ _ _ _ _ loc) = locOf loc

instance Located Definition where
    locOf (FuncDef _ loc)     = locOf loc
    locOf (DecDef _ loc)      = locOf loc
    locOf (EscDef _ loc)      = locOf loc
    locOf (AntiFunc _ loc)    = locOf loc
    locOf (AntiEsc _ loc)     = locOf loc
    locOf (AntiEdecl _ loc)   = locOf loc
    locOf (AntiEdecls _ loc)  = locOf loc

instance Located Stm where
    locOf (Label _ _ loc)       = locOf loc
    locOf (Case _ _ loc)        = locOf loc
    locOf (Default _ loc)       = locOf loc
    locOf (Exp _ loc)           = locOf loc
    locOf (Block _ loc)         = locOf loc
    locOf (If _ _ _ loc)        = locOf loc
    locOf (Switch _ _ loc)      = locOf loc
    locOf (While _ _ loc)       = locOf loc
    locOf (DoWhile _ _ loc)     = locOf loc
    locOf (For _ _ _ _ loc)     = locOf loc
    locOf (Goto _ loc)          = locOf loc
    locOf (Continue loc)        = locOf loc
    locOf (Break loc)           = locOf loc
    locOf (Return _ loc)        = locOf loc
    locOf (Pragma _ loc)        = locOf loc
    locOf (Asm _ _ _ _ _ _ loc) = locOf loc
    locOf (AntiPragma _ loc)    = locOf loc
    locOf (AntiStm _ loc)       = locOf loc
    locOf (AntiStms _ loc)      = locOf loc

instance Located BlockItem where
    locOf (BlockDecl decl)       = locOf decl
    locOf (BlockStm stm)         = locOf stm
    locOf (AntiBlockItem _ loc)  = locOf loc
    locOf (AntiBlockItems _ loc) = locOf loc

instance Located Const where
    locOf (IntConst _ _ _ loc)          = locOf loc
    locOf (LongIntConst _ _ _ loc)      = locOf loc
    locOf (LongLongIntConst _ _ _ loc)  = locOf loc
    locOf (FloatConst _ _ loc)          = locOf loc
    locOf (DoubleConst _ _ loc)         = locOf loc
    locOf (LongDoubleConst _ _ loc)     = locOf loc
    locOf (CharConst _ _ loc)           = locOf loc
    locOf (StringConst _ _ loc)         = locOf loc
    locOf (AntiInt _ loc)               = locOf loc
    locOf (AntiUInt _ loc)              = locOf loc
    locOf (AntiLInt _ loc)              = locOf loc
    locOf (AntiULInt _ loc)             = locOf loc
    locOf (AntiLLInt _ loc)             = locOf loc
    locOf (AntiULLInt _ loc)            = locOf loc
    locOf (AntiFloat _ loc)             = locOf loc
    locOf (AntiDouble _ loc)            = locOf loc
    locOf (AntiLongDouble _ loc)        = locOf loc
    locOf (AntiChar _ loc)              = locOf loc
    locOf (AntiString _ loc)            = locOf loc

instance Located ExeConfig where
    locOf conf = locOf (exeLoc conf)

instance Located Exp where
    locOf (Var _ loc)             = locOf loc
    locOf (Const _ loc)           = locOf loc
    locOf (BinOp _ _ _ loc)       = locOf loc
    locOf (Assign _ _ _ loc)      = locOf loc
    locOf (PreInc _ loc)          = locOf loc
    locOf (PostInc _ loc)         = locOf loc
    locOf (PreDec _ loc)          = locOf loc
    locOf (PostDec _ loc)         = locOf loc
    locOf (UnOp _ _ loc)          = locOf loc
    locOf (SizeofExp _ loc)       = locOf loc
    locOf (SizeofType _ loc)      = locOf loc
    locOf (Cast _ _ loc)          = locOf loc
    locOf (Cond _ _ _ loc)        = locOf loc
    locOf (Member _ _ loc)        = locOf loc
    locOf (PtrMember _ _ loc)     = locOf loc
    locOf (Index _ _ loc)         = locOf loc
    locOf (FnCall _ _ loc)        = locOf loc
    locOf (CudaCall _ _ _ loc)    = locOf loc
    locOf (Seq _ _ loc)           = locOf loc
    locOf (CompoundLit _ _ loc)   = locOf loc
    locOf (StmExpr _ loc)         = locOf loc
    locOf (BuiltinVaArg _ _ loc)  = locOf loc
    locOf (AntiExp _ loc)         = locOf loc
    locOf (AntiArgs _ loc)        = locOf loc

ctypedef :: Id -> Decl -> [Attr] -> Typedef
ctypedef id decl attrs =
    Typedef id decl attrs (id `srcspan` decl `srcspan` attrs)

cdeclSpec :: [Storage] -> [TypeQual] -> TypeSpec -> DeclSpec
cdeclSpec storage quals spec =
    DeclSpec storage quals spec (storage `srcspan` quals `srcspan` spec)

cinitGroup :: DeclSpec -> [Attr] -> [Init] -> InitGroup
cinitGroup dspec attrs inis =
    InitGroup dspec attrs inis (dspec `srcspan` attrs `srcspan` inis)

ctypedefGroup :: DeclSpec -> [Attr] -> [Typedef] -> InitGroup
ctypedefGroup dspec attrs typedefs =
    TypedefGroup dspec attrs typedefs (dspec `srcspan` attrs `srcspan` typedefs)
