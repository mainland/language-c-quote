-- |
-- Module      :  Language.C.Syntax
-- Copyright   :  (c) 2006-2011 Harvard University
--                (c) 2011-2013 Geoffrey Mainland
--                (c) 2013 Manuel M T Chakravarty
--             :  (c) 2013-2015 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@cs.drexel.edu

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.C.Syntax where

import Data.Data (Data(..))
import Data.Loc
import Data.String (IsString(..))
import Data.Typeable (Typeable)

data Extensions = Antiquotation
                | C99
                | C11
                | Gcc
                | Blocks
                | ObjC
                | CUDA
                | OpenCL
  deriving (Eq, Ord, Enum, Show)

data Id = Id     String !SrcLoc
        | AntiId String !SrcLoc
    deriving (Eq, Ord, Show, Data, Typeable)

data StringLit = StringLit [String] String !SrcLoc
    deriving (Eq, Ord, Show, Data, Typeable)

type Linkage = StringLit

data Storage = Tauto                   !SrcLoc
             | Tregister               !SrcLoc
             | Tstatic                 !SrcLoc
             | Textern (Maybe Linkage) !SrcLoc
             | Ttypedef                !SrcLoc

             -- Clang blocks
             | T__block !SrcLoc

             -- Objective-C
             | TObjC__weak              !SrcLoc
             | TObjC__strong            !SrcLoc
             | TObjC__unsafe_unretained !SrcLoc
    deriving (Eq, Ord, Show, Data, Typeable)

data TypeQual = Tconst    !SrcLoc
              | Tvolatile !SrcLoc

              | AntiTypeQual  String !SrcLoc
              | AntiTypeQuals String !SrcLoc

              -- C99
              | Tinline   !SrcLoc
              | Trestrict !SrcLoc

              -- GCC
              | TAttr Attr

              -- CUDA
              | TCUDAdevice   !SrcLoc
              | TCUDAglobal   !SrcLoc
              | TCUDAhost     !SrcLoc
              | TCUDAconstant !SrcLoc
              | TCUDAshared   !SrcLoc
              | TCUDArestrict !SrcLoc
              | TCUDAnoinline !SrcLoc

              -- OpenCL
              | TCLprivate   !SrcLoc
              | TCLlocal     !SrcLoc
              | TCLglobal    !SrcLoc
              | TCLconstant  !SrcLoc
              | TCLreadonly  !SrcLoc
              | TCLwriteonly !SrcLoc
              | TCLkernel    !SrcLoc
    deriving (Eq, Ord, Show, Data, Typeable)

data Sign = Tsigned   !SrcLoc
          | Tunsigned !SrcLoc
    deriving (Eq, Ord, Show, Data, Typeable)

data TypeSpec = Tvoid                   !SrcLoc
              | Tchar      (Maybe Sign) !SrcLoc
              | Tshort     (Maybe Sign) !SrcLoc
              | Tint       (Maybe Sign) !SrcLoc
              | Tlong      (Maybe Sign) !SrcLoc
              | Tlong_long (Maybe Sign) !SrcLoc
              | Tfloat                  !SrcLoc
              | Tdouble                 !SrcLoc
              | Tlong_double            !SrcLoc
              | Tstruct (Maybe Id) (Maybe [FieldGroup]) [Attr] !SrcLoc
              | Tunion  (Maybe Id) (Maybe [FieldGroup]) [Attr] !SrcLoc
              | Tenum   (Maybe Id) [CEnum]              [Attr] !SrcLoc
              | Tnamed Id       -- A typedef name
                       [Id]     -- Objective-C protocol references
                       !SrcLoc

              -- C99
              | T_Bool                 !SrcLoc
              | Tfloat_Complex         !SrcLoc
              | Tdouble_Complex        !SrcLoc
              | Tlong_double_Complex   !SrcLoc
              | Tfloat_Imaginary       !SrcLoc
              | Tdouble_Imaginary      !SrcLoc
              | Tlong_double_Imaginary !SrcLoc

              -- Gcc
              | TtypeofExp  Exp  !SrcLoc
              | TtypeofType Type !SrcLoc
              | Tva_list         !SrcLoc
    deriving (Eq, Ord, Show, Data, Typeable)

data DeclSpec = DeclSpec         [Storage] [TypeQual] TypeSpec !SrcLoc
              | AntiDeclSpec                          String   !SrcLoc
              | AntiTypeDeclSpec [Storage] [TypeQual] String   !SrcLoc
    deriving (Eq, Ord, Show, Data, Typeable)

-- | There are two types of declarators in C, regular declarators and abstract
-- declarators. The former is for declaring variables, function parameters,
-- typedefs, etc. and the latter for abstract types---@typedef int
-- ({*}foo)(void)@ vs. @\tt int ({*})(void)@. The difference between the two is
-- just whether or not an identifier is attached to the declarator. We therefore
-- only define one 'Decl' type and use it for both cases.

data ArraySize = ArraySize Bool Exp !SrcLoc
               | VariableArraySize !SrcLoc
               | NoArraySize !SrcLoc
    deriving (Eq, Ord, Show, Data, Typeable)

data Decl = DeclRoot !SrcLoc
          | Ptr [TypeQual] Decl !SrcLoc
          | Array [TypeQual] ArraySize Decl !SrcLoc
          | Proto Decl Params !SrcLoc
          | OldProto Decl [Id] !SrcLoc
          | AntiTypeDecl String !SrcLoc

          -- Clang blocks
          | BlockPtr [TypeQual] Decl !SrcLoc
    deriving (Eq, Ord, Show, Data, Typeable)

data Type = Type DeclSpec Decl !SrcLoc
          | AntiType String !SrcLoc
    deriving (Eq, Ord, Show, Data, Typeable)

data Designator = IndexDesignator Exp !SrcLoc
                | MemberDesignator Id !SrcLoc
    deriving (Eq, Ord, Show, Data, Typeable)

data Designation = Designation [Designator] !SrcLoc
    deriving (Eq, Ord, Show, Data, Typeable)

data Initializer = ExpInitializer Exp !SrcLoc
                 | CompoundInitializer [(Maybe Designation, Initializer)] !SrcLoc
                 | AntiInit  String !SrcLoc
                 | AntiInits String !SrcLoc
    deriving (Eq, Ord, Show, Data, Typeable)

type AsmLabel = StringLit

data Init = Init Id Decl (Maybe AsmLabel) (Maybe Initializer) [Attr] !SrcLoc
    deriving (Eq, Ord, Show, Data, Typeable)

data Typedef = Typedef Id Decl [Attr] !SrcLoc
    deriving (Eq, Ord, Show, Data, Typeable)

data InitGroup = InitGroup    DeclSpec [Attr] [Init]    !SrcLoc
               | TypedefGroup DeclSpec [Attr] [Typedef] !SrcLoc
               | AntiDecl  String !SrcLoc
               | AntiDecls String !SrcLoc
    deriving (Eq, Ord, Show, Data, Typeable)

data Field = Field (Maybe Id) (Maybe Decl) (Maybe Exp) !SrcLoc
    deriving (Eq, Ord, Show, Data, Typeable)

data FieldGroup  =  FieldGroup DeclSpec [Field] !SrcLoc
                 |  AntiSdecl  String !SrcLoc
                 |  AntiSdecls String !SrcLoc
    deriving (Eq, Ord, Show, Data, Typeable)

data CEnum  =  CEnum Id (Maybe Exp) !SrcLoc
            |  AntiEnum  String !SrcLoc
            |  AntiEnums String !SrcLoc
    deriving (Eq, Ord, Show, Data, Typeable)

data Attr  =  Attr Id [Exp] !SrcLoc
    deriving (Eq, Ord, Show, Data, Typeable)

data Param  =  Param (Maybe Id) DeclSpec Decl !SrcLoc
            |  AntiParam  String !SrcLoc
            |  AntiParams String !SrcLoc
    deriving (Eq, Ord, Show, Data, Typeable)

data Params = Params [Param] Bool !SrcLoc
    deriving (Eq, Ord, Show, Data, Typeable)

data Func  =  Func    DeclSpec Id Decl Params                   [BlockItem] !SrcLoc
           |  OldFunc DeclSpec Id Decl [Id] (Maybe [InitGroup]) [BlockItem] !SrcLoc
    deriving (Eq, Ord, Show, Data, Typeable)

data Definition  =  FuncDef    Func      !SrcLoc
                 |  DecDef     InitGroup !SrcLoc
                 |  EscDef     String    !SrcLoc
                 |  AntiFunc   String    !SrcLoc
                 |  AntiEsc    String    !SrcLoc
                 |  AntiEdecl  String    !SrcLoc
                 |  AntiEdecls String    !SrcLoc

                 -- Objective-C
                 |  ObjCClassDec   [Id] !SrcLoc
                 |  ObjCClassIface Id (Maybe Id) [Id] [ObjCIvarDecl] [ObjCIfaceDecl] [Attr] !SrcLoc
                 |  ObjCCatIface   Id (Maybe Id) [Id] [ObjCIvarDecl] [ObjCIfaceDecl]        !SrcLoc
                 |  ObjCProtDec    [Id] !SrcLoc
                 |  ObjCProtDef    Id [Id] [ObjCIfaceDecl] !SrcLoc
                 |  ObjCClassImpl  Id (Maybe Id) [ObjCIvarDecl] [Definition] !SrcLoc
                 |  ObjCCatImpl    Id Id [Definition] !SrcLoc
                 |  ObjCSynDef     [(Id, Maybe Id)] !SrcLoc
                 |  ObjCDynDef     [Id] !SrcLoc
                 |  ObjCMethDef    ObjCMethodProto [BlockItem] !SrcLoc
                 |  ObjCCompAlias  Id Id !SrcLoc

                 |  AntiObjCMeth  String !SrcLoc
                 |  AntiObjCMeths String !SrcLoc
    deriving (Eq, Ord, Show, Data, Typeable)

data Stm  = Label Id [Attr] Stm !SrcLoc
          | Case Exp Stm !SrcLoc
          | Default Stm !SrcLoc
          | Exp (Maybe Exp) !SrcLoc
          | Block [BlockItem] !SrcLoc
          | If Exp Stm (Maybe Stm) !SrcLoc
          | Switch Exp Stm !SrcLoc
          | While Exp Stm !SrcLoc
          | DoWhile Stm Exp !SrcLoc
          | For (Either InitGroup (Maybe Exp)) (Maybe Exp) (Maybe Exp) Stm !SrcLoc
          | Goto Id !SrcLoc
          | Continue !SrcLoc
          | Break !SrcLoc
          | Return (Maybe Exp) !SrcLoc
          | Pragma String !SrcLoc
          | Comment String Stm !SrcLoc
          | AntiPragma String !SrcLoc
          | AntiComment String Stm !SrcLoc
          | AntiStm String !SrcLoc
          | AntiStms String !SrcLoc

          -- GCC
          | Asm Bool         -- @True@ if volatile, @False@ otherwise
                [Attr]       -- Attributes
                AsmTemplate  -- Assembly template
                [AsmOut]     -- Output operands
                [AsmIn]      -- Input operands
                [AsmClobber] -- Clobbered registers
                !SrcLoc
          | AsmGoto Bool         -- @True@ if volatile, @False@ otherwise
                    [Attr]       -- Attributes
                    AsmTemplate  -- Assembly template
                    [AsmIn]      -- Input operands
                    [AsmClobber] -- Clobbered registers
                    [Id]         -- Labels
                    !SrcLoc

          -- Objective-C
          | ObjCTry [BlockItem] [ObjCCatch] (Maybe [BlockItem]) !SrcLoc
            -- ^Invariant: There is either at least one 'ObjCCatch' or the finally block is present.
          | ObjCThrow (Maybe Exp) !SrcLoc
          | ObjCSynchronized Exp [BlockItem] !SrcLoc
          | ObjCAutoreleasepool [BlockItem] !SrcLoc
    deriving (Eq, Ord, Show, Data, Typeable)

data BlockItem = BlockDecl InitGroup
               | BlockStm Stm
               | AntiBlockItem  String !SrcLoc
               | AntiBlockItems String !SrcLoc
    deriving (Eq, Ord, Show, Data, Typeable)

data Signed = Signed
            | Unsigned
    deriving (Eq, Ord, Show, Data, Typeable)

-- | The 'String' parameter to 'Const' data constructors is the raw string
-- representation of the constant as it was parsed.
data Const = IntConst         String   Signed Integer !SrcLoc
           | LongIntConst     String   Signed Integer !SrcLoc
           | LongLongIntConst String   Signed Integer !SrcLoc
           | FloatConst       String   Rational       !SrcLoc
           | DoubleConst      String   Rational       !SrcLoc
           | LongDoubleConst  String   Rational       !SrcLoc
           | CharConst        String   Char           !SrcLoc
           | StringConst      [String] String         !SrcLoc

           | AntiConst      String !SrcLoc
           | AntiInt        String !SrcLoc
           | AntiUInt       String !SrcLoc
           | AntiLInt       String !SrcLoc
           | AntiULInt      String !SrcLoc
           | AntiLLInt      String !SrcLoc
           | AntiULLInt     String !SrcLoc
           | AntiFloat      String !SrcLoc
           | AntiDouble     String !SrcLoc
           | AntiLongDouble String !SrcLoc
           | AntiChar       String !SrcLoc
           | AntiString     String !SrcLoc
    deriving (Eq, Ord, Show, Data, Typeable)

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
         | AntiExp String !SrcLoc
         | AntiArgs String !SrcLoc

         -- GCC
         | BuiltinVaArg Exp Type !SrcLoc

         -- Clang blocks
         | BlockLit BlockType [Attr] [BlockItem] !SrcLoc

         -- Objective-C
         | ObjCMsg ObjCRecv [ObjCArg] [Exp] !SrcLoc
           -- ^Invariant: First argument must at least have either a selector or an expression;
           --  all other arguments must have an expression.
         | ObjCLitConst (Maybe UnOp)
                        Const        -- Anything except 'StringConst'
                        !SrcLoc
         | ObjCLitString [Const] -- Must all be 'StringConst'
                         !SrcLoc
         | ObjCLitBool Bool !SrcLoc
         | ObjCLitArray [Exp] !SrcLoc
         | ObjCLitDict [ObjCDictElem] !SrcLoc
         | ObjCLitBoxed Exp !SrcLoc
         | ObjCEncode Type !SrcLoc
         | ObjCProtocol Id !SrcLoc
         | ObjCSelector String !SrcLoc

         -- CUDA: C++11 lambda-expression
         | Lambda LambdaIntroducer (Maybe LambdaDeclarator) [BlockItem] !SrcLoc
    deriving (Eq, Ord, Show, Data, Typeable)

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
    deriving (Eq, Ord, Show, Data, Typeable)

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
    deriving (Eq, Ord, Show, Data, Typeable)

data UnOp = AddrOf
          | Deref
          | Positive
          | Negate
          | Not
          | Lnot
    deriving (Eq, Ord, Show, Data, Typeable)

{------------------------------------------------------------------------------
 -
 - GCC extensions
 -
 ------------------------------------------------------------------------------}

type AsmTemplate = StringLit

data AsmOut = AsmOut (Maybe Id) String Id
    deriving (Eq, Ord, Show, Data, Typeable)

data AsmIn = AsmIn (Maybe Id) String Exp
    deriving (Eq, Ord, Show, Data, Typeable)

type AsmClobber = String

{------------------------------------------------------------------------------
 -
 - Clang blocks
 -
 ------------------------------------------------------------------------------}
data BlockType = BlockVoid !SrcLoc
               | BlockParam [Param] !SrcLoc
               | BlockType Type !SrcLoc
                 -- NB: Type may be something other than 'Proto', in which case clang defaults to
                 --     regard the type as the return type and assume the arguments to be 'void'.
    deriving (Eq, Ord, Show, Data, Typeable)

{------------------------------------------------------------------------------
 -
 - Objective-C
 -
 ------------------------------------------------------------------------------}

data ObjCIvarDecl = ObjCIvarVisi ObjCVisibilitySpec !SrcLoc
                  | ObjCIvarDecl FieldGroup !SrcLoc
                  -- -=chak FIXME: needs ANTI forms
    deriving (Eq, Ord, Show, Data, Typeable)

data ObjCVisibilitySpec = ObjCPrivate !SrcLoc
                        | ObjCPublic !SrcLoc
                        | ObjCProtected !SrcLoc
                        | ObjCPackage !SrcLoc
    deriving (Eq, Ord, Show, Data, Typeable)

data ObjCIfaceDecl = ObjCIfaceProp [ObjCPropAttr] FieldGroup !SrcLoc
                   | ObjCIfaceReq ObjCMethodReq !SrcLoc
                   | ObjCIfaceMeth ObjCMethodProto !SrcLoc
                   | ObjCIfaceDecl InitGroup !SrcLoc

                   | AntiObjCProp       String !SrcLoc
                   | AntiObjCProps      String !SrcLoc
                   | AntiObjCIfaceDecl  String !SrcLoc
                   | AntiObjCIfaceDecls String !SrcLoc
    deriving (Eq, Ord, Show, Data, Typeable)

data ObjCPropAttr = ObjCGetter Id !SrcLoc
                  | ObjCSetter Id !SrcLoc
                  | ObjCReadonly !SrcLoc
                  | ObjCReadwrite !SrcLoc
                  | ObjCAssign !SrcLoc
                  | ObjCRetain !SrcLoc
                  | ObjCCopy !SrcLoc
                  | ObjCNonatomic !SrcLoc
                  | ObjCAtomic !SrcLoc
                  | ObjCStrong !SrcLoc
                  | ObjCWeak !SrcLoc
                  | ObjCUnsafeUnretained !SrcLoc

                  | AntiObjCAttr  String !SrcLoc
                  | AntiObjCAttrs String !SrcLoc
    deriving (Eq, Ord, Show, Data, Typeable)

data ObjCMethodReq = ObjCRequired !SrcLoc
                   | ObjCOptional !SrcLoc
    deriving (Eq, Ord, Show, Data, Typeable)

data ObjCParam = ObjCParam (Maybe Id) (Maybe Type) [Attr] (Maybe Id) !SrcLoc
               | AntiObjCParam  String !SrcLoc
               | AntiObjCParams String !SrcLoc
    deriving (Eq, Ord, Show, Data, Typeable)

data ObjCMethodProto = ObjCMethodProto Bool (Maybe Type) [Attr] [ObjCParam] Bool [Attr] !SrcLoc
                       -- ^Invariant: First parameter must at least either have a selector or
                       --  an identifier; all other parameters must have an identifier.
                     | AntiObjCMethodProto String !SrcLoc
    deriving (Eq, Ord, Show, Data, Typeable)

data ObjCCatch = ObjCCatch (Maybe Param) [BlockItem] !SrcLoc
    deriving (Eq, Ord, Show, Data, Typeable)

data ObjCDictElem = ObjCDictElem Exp Exp !SrcLoc
                  | AntiObjCDictElems String !SrcLoc
    deriving (Eq, Ord, Show, Data, Typeable)

data ObjCRecv = ObjCRecvSuper !SrcLoc
              | ObjCRecvExp Exp !SrcLoc
              | AntiObjCRecv String !SrcLoc
    deriving (Eq, Ord, Show, Data, Typeable)

data ObjCArg = ObjCArg (Maybe Id) (Maybe Exp) !SrcLoc
             | AntiObjCArg String !SrcLoc
             | AntiObjCArgs String !SrcLoc
    deriving (Eq, Ord, Show, Data, Typeable)

{------------------------------------------------------------------------------
 -
 - CUDA
 -
 ------------------------------------------------------------------------------}

data LambdaIntroducer = LambdaIntroducer [CaptureListEntry] !SrcLoc
    deriving (Eq, Ord, Show, Data, Typeable)

data LambdaDeclarator = LambdaDeclarator Params Bool (Maybe Type) !SrcLoc
    deriving (Eq, Ord, Show, Data, Typeable)

data CaptureListEntry = DefaultByReference
                      | DefaultByValue
    deriving (Eq, Ord, Show, Data, Typeable)

data ExeConfig = ExeConfig
    {  exeGridDim    :: Exp
    ,  exeBlockDim   :: Exp
    ,  exeSharedSize :: Maybe Exp
    ,  exeStream     :: Maybe Exp
    ,  exeLoc        :: !SrcLoc
    }
    deriving (Eq, Ord, Show, Data, Typeable)

{------------------------------------------------------------------------------
 -
 - Instances
 -
 ------------------------------------------------------------------------------}

instance IsString Id where
    fromString s = Id s noLoc

instance IsString StringLit where
    fromString s = StringLit [s] s noLoc

#if !defined(ONLY_TYPEDEFS)
#include "Language/C/Syntax-instances.hs"

{------------------------------------------------------------------------------
 -
 - Utilities
 -
 ------------------------------------------------------------------------------}

funcProto :: Func -> InitGroup
funcProto f@(Func decl_spec ident decl params _ _) =
    InitGroup decl_spec []
      [Init ident (Proto decl params l) Nothing Nothing [] l] l
  where
    l = srclocOf f

funcProto f@(OldFunc decl_spec ident decl params _ _ _) =
    InitGroup decl_spec []
      [Init ident (OldProto decl params l) Nothing Nothing [] l] l
  where
    l = srclocOf f

isPtr :: Type -> Bool
isPtr  (Type _ decl _)  = go decl
  where
    go  (DeclRoot _)        = False
    go  (Ptr _ _ _)         = True
    go  (BlockPtr _ _ _)    = True
    go  (Array _ _ _ _)     = True
    go  (Proto _ _ _)       = False
    go  (OldProto _ _ _)    = False
    go  (AntiTypeDecl _ _)  = error "isPtr: encountered antiquoted type declaration"
isPtr  (AntiType _ _)       = error "isPtr: encountered antiquoted type"

ctypedef :: Id -> Decl -> [Attr] -> Typedef
ctypedef ident decl attrs =
    Typedef ident decl attrs (ident `srcspan` decl `srcspan` attrs)

cdeclSpec :: [Storage] -> [TypeQual] -> TypeSpec -> DeclSpec
cdeclSpec storage quals spec =
    DeclSpec storage quals spec (storage `srcspan` quals `srcspan` spec)

cinitGroup :: DeclSpec -> [Attr] -> [Init] -> InitGroup
cinitGroup dspec attrs inis =
    InitGroup dspec attrs inis (dspec `srcspan` attrs `srcspan` inis)

ctypedefGroup :: DeclSpec -> [Attr] -> [Typedef] -> InitGroup
ctypedefGroup dspec attrs typedefs =
    TypedefGroup dspec attrs typedefs (dspec `srcspan` attrs `srcspan` typedefs)
#endif /* !defined(ONLY_TYPEDEFS) */
