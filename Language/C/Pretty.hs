{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      :  Language.C.Pretty
-- Copyright   :  (c) Harvard University 2006-2011
--                (c) Geoffrey Mainland 2011-2013
--             :  (c) Drexel University 2013
-- License     :  BSD-style
-- Maintainer  :  mainland@cs.drexel.edu

module Language.C.Pretty where

import Data.Char (isAlphaNum,
                  isLower)
import Data.Loc
import Language.C.Syntax
import Text.PrettyPrint.Mainland

pprLoc :: SrcLoc -> Doc -> Doc
pprLoc loc doc = srcloc loc <> doc

data Fixity = Fixity Assoc Int
  deriving (Eq, Ord)

data Assoc = LeftAssoc | RightAssoc | NonAssoc
  deriving (Eq, Ord)

infix_ :: Int -> Fixity
infix_ = Fixity NonAssoc

infixl_ :: Int -> Fixity
infixl_ = Fixity LeftAssoc

infixr_ :: Int -> Fixity
infixr_ = Fixity RightAssoc

infixop :: (Pretty a, Pretty b, Pretty op, CFixity op)
        => Int -- ^ precedence of context
        -> op  -- ^ operator
        -> a   -- ^ left argument
        -> b   -- ^ right argument
        -> Doc
infixop prec op l r =
    parensIf (prec > opPrec) $
    pprPrec leftPrec l <+> ppr op <+/> pprPrec rightPrec r
  where
    leftPrec | opAssoc == RightAssoc = opPrec + 1
             | otherwise             = opPrec

    rightPrec | opAssoc == LeftAssoc = opPrec + 1
              | otherwise            = opPrec

    Fixity opAssoc opPrec = fixity op

parensList :: [Doc] -> Doc
parensList = encloseSep lparen rparen comma

bracesList :: [Doc] -> Doc
bracesList = encloseSep lbrace rbrace comma

bracesSemiList :: [Doc] -> Doc
bracesSemiList = encloseSep lbrace rbrace semi

angleList :: [Doc] -> Doc
angleList = encloseSep langle rangle comma

pprAnti :: String -> String -> Doc
pprAnti anti s = char '$' <> text anti <> colon <>
                 if isIdentifier s then text s else parens (text s)
  where
    isIdentifier :: String -> Bool
    isIdentifier []       = False
    isIdentifier ('_':cs) = all isIdChar cs
    isIdentifier (c:cs)   = isLower c && all isIdChar cs

    isIdChar :: Char -> Bool
    isIdChar '_' = True
    isIdChar c   = isAlphaNum c

class CFixity a where
    fixity :: a -> Fixity

-- Fixities are taken from Table 2-1 in Section 2.12 of K&R (2nd ed.)

instance CFixity BinOp where
    fixity Add  = infixl_ 12
    fixity Sub  = infixl_ 12
    fixity Mul  = infixl_ 13
    fixity Div  = infixl_ 13
    fixity Mod  = infixl_ 13
    fixity Eq   = infixl_ 9
    fixity Ne   = infixl_ 9
    fixity Lt   = infixl_ 10
    fixity Gt   = infixl_ 10
    fixity Le   = infixl_ 10
    fixity Ge   = infixl_ 10
    fixity Land = infixl_ 5
    fixity Lor  = infixl_ 4
    fixity And  = infixl_ 8
    fixity Or   = infixl_ 6
    fixity Xor  = infixl_ 7
    fixity Lsh  = infixl_ 11
    fixity Rsh  = infixl_ 11

instance CFixity AssignOp where
    fixity _ = infixr_ 2

instance CFixity UnOp where
    fixity _ = infixr_ 14

instance Pretty Id where
    ppr (Id ident _)  = text ident
    ppr (AntiId v _)  = pprAnti "id" v

instance Pretty Storage where
    ppr (Tauto _)                  = text "auto"
    ppr (Tregister _)              = text "register"
    ppr (Tstatic _)                = text "static"
    ppr (Textern _)                = text "extern"
    ppr (TexternL l _)             = text "extern" <+> ppr l
    ppr (Ttypedef _)               = text "typedef"
    ppr (T__block _)               = text "__block"
    ppr (TObjC__weak _)            = text "__weak"
    ppr (TObjC__strong _)          = text "__strong"
    ppr (TObjC__unsafe_retained _) = text "__unsafe_retained"

instance Pretty TypeQual where
    ppr (Tconst _)        = text "const"
    ppr (Tvolatile _)     = text "volatile"
    ppr (Tinline _)       = text "inline"

    ppr (Trestrict _)     = text "__restrict"

    ppr (TCUDAdevice _)   = text "__device__"
    ppr (TCUDAglobal _)   = text "__global__"
    ppr (TCUDAhost _)     = text "__host__"
    ppr (TCUDAconstant _) = text "__constant__"
    ppr (TCUDAshared _)   = text "__shared__"
    ppr (TCUDArestrict _) = text "__restrict__"
    ppr (TCUDAnoinline _) = text "__noinline__"

    ppr (TCLprivate _)    = text "__private"
    ppr (TCLlocal _)      = text "__local"
    ppr (TCLglobal _)     = text "__global"
    ppr (TCLconstant _)   = text "__constant"
    ppr (TCLreadonly _)   = text "read_only"
    ppr (TCLwriteonly _)  = text "write_only"
    ppr (TCLkernel _)     = text "__kernel"

instance Pretty Sign where
    ppr (Tsigned _)    = text "signed"
    ppr (Tunsigned _)  = text "unsigned"

pprSign :: Maybe Sign -> Doc
pprSign Nothing     = empty
pprSign (Just sign) = ppr sign <> space

instance Pretty TypeSpec where
    ppr (Tvoid _)            = text "void"
    ppr (Tchar sign _)       = pprSign sign <> text "char"
    ppr (Tshort sign _)      = pprSign sign <> text "short"
    ppr (Tint sign _)        = pprSign sign <> text "int"
    ppr (Tlong sign _)       = pprSign sign <> text "long"
    ppr (Tlong_long sign _)  = pprSign sign <> text "long long"
    ppr (Tfloat _)           = text "float"
    ppr (Tdouble _)          = text "double"
    ppr (Tlong_double _)     = text "long double"

    ppr (Tstruct maybe_ident maybe_fields attrs _) =
        pprStructOrUnion "struct" maybe_ident maybe_fields attrs

    ppr (Tunion maybe_ident maybe_fields attrs _) =
        pprStructOrUnion "union" maybe_ident maybe_fields attrs

    ppr (Tenum maybe_ident cenums attrs _) =
        pprEnum maybe_ident cenums attrs

    ppr (Tnamed ident refs _) =
        ppr ident <> if null refs then empty else angles (commasep (map ppr refs))

    ppr (TtypeofExp e _) =
        text "__typeof__" <> parens (pprPrec 14 e)

    ppr (TtypeofType tipe _) =
        text "__typeof__" <> parens (ppr tipe)

    ppr (Tva_list _) =
        text "__builtin_va_list"

pprStructOrUnion :: String
                 -> Maybe Id
                 -> Maybe [FieldGroup]
                 -> [Attr]
                 -> Doc
pprStructOrUnion ty maybe_ident maybe_fields attrs =
    text ty
    <> case maybe_ident of
         Nothing ->    empty
         Just ident -> space <> ppr ident
    <> case maybe_fields of
           Nothing ->     empty
           Just fields -> space <> lbrace
                          <> nest 4 (line <> stack (zipWith (<>) (map ppr fields) (repeat semi)))
                          </> rbrace
    <> case attrs of
         [] -> empty
         _ ->  softline <> ppr attrs

pprEnum :: Maybe Id
        -> [CEnum]
        -> [Attr]
        -> Doc
pprEnum maybe_ident cenums attrs =
    text "enum"
    <> case maybe_ident of
         Nothing ->    empty
         Just ident -> space <> ppr ident
    <> case cenums of
         [] -> empty
         _  -> space <> lbrace <>
               nest 4 (line <> stack (punctuate comma (map ppr cenums))) </>
               rbrace
    <> case attrs of
         [] -> empty
         _ ->  softline <> ppr attrs

instance Pretty DeclSpec where
    ppr (DeclSpec storage quals spec _) =
        case map ppr storage ++ map ppr quals of
          [] ->   ppr spec
          docs -> spread docs <+/> ppr spec

    ppr (AntiDeclSpec v _) =
        pprAnti "spec" v

    ppr (AntiTypeDeclSpec storage quals v _) =
        spread (map ppr storage ++ map ppr quals) <+/>
        pprAnti "ty" v

instance Pretty ArraySize where
    ppr (ArraySize True e _)  = text "static" <+> ppr e
    ppr (ArraySize False e _) = ppr e
    ppr (VariableArraySize _) = text "*"
    ppr (NoArraySize _)       = empty

pprDeclarator :: Maybe Id -> Decl -> Doc
pprDeclarator maybe_ident declarator =
    case maybe_ident of
      Nothing ->    pprDecl declarator empty
      Just ident -> pprDecl declarator (space <> ppr ident)
    where
      pprPtr :: Decl -> Doc -> (Decl, Doc)
      pprPtr (Ptr [] decl _) post =
          pprPtr decl $
          text "*" <> post
      pprPtr (Ptr quals decl _) post =
          pprPtr decl $
          text "*" <+> spread (map ppr quals) <> post
      pprPtr (BlockPtr [] decl _) post =
          pprPtr decl $
          text "^" <> post
      pprPtr (BlockPtr quals decl _) post =
          pprPtr decl $
          text "^" <+> spread (map ppr quals) <> post
      pprPtr decl post = (decl, post)

      pprDirDecl :: Decl -> Doc -> (Decl, Doc)
      pprDirDecl (Array [] size decl _) pre =
          pprDirDecl decl $
          pre <> brackets (align (ppr size))

      pprDirDecl (Array quals size decl _) pre =
          pprDirDecl decl $
          pre <> brackets (align (spread (map ppr quals) <> ppr size))

      pprDirDecl (Proto decl args _) pre =
          pprDirDecl decl $
          pre <> parens (ppr args)

      pprDirDecl (OldProto decl args _) pre =
          pprDirDecl decl $
          pre <> parensList (map ppr args)

      pprDirDecl decl pre = (decl, pre)

      pprDecl :: Decl -> Doc -> Doc
      pprDecl decl mid =
          case decl' of
            DeclRoot _  -> declDoc
            _           -> pprDecl decl' (parens declDoc)
        where
          (decl', declDoc) = uncurry pprPtr (pprDirDecl decl mid)

instance Pretty Type where
    ppr (Type spec decl _)  = ppr spec <> pprDeclarator Nothing decl
    ppr (AntiType v _)      = pprAnti "ty" v

instance Pretty Designator where
    ppr (IndexDesignator e _)       = brackets $ ppr e
    ppr (MemberDesignator ident _)  = dot <> ppr ident

instance Pretty Designation where
    ppr (Designation ds _) = folddoc (<>) (map ppr ds)

instance Pretty Initializer where
    ppr (ExpInitializer e _) = ppr e

    ppr (CompoundInitializer inits _) =
        bracesList (map pprInit inits)
      where
        pprInit :: (Maybe Designation, Initializer) -> Doc
        pprInit (Nothing, ini) = ppr ini
        pprInit (Just d, ini)  = ppr d <+> text "=" <//> ppr ini

    ppr (AntiInit v _)  = pprAnti "init" v
    ppr (AntiInits v _) = pprAnti "inits" v

instance Pretty Init where
    ppr (Init ident decl maybe_asmlabel maybe_e attrs _) =
        pprDeclarator (Just ident) decl
        <> case attrs of
             [] -> empty
             _ ->  softline <> ppr attrs
        <> case maybe_asmlabel of
             Nothing -> empty
             Just l ->  space <> text "asm" <+> parens (text l)
        <> case maybe_e of
             Nothing -> empty
             Just e ->  space <> text "=" <+/> ppr e

instance Pretty Typedef where
    ppr (Typedef ident decl attrs loc) =
        ppr (Init ident decl Nothing Nothing attrs loc)

instance Pretty InitGroup where
    ppr (InitGroup spec attrs inits _) =
        ppr spec
        <> case attrs of
             [] -> empty
             _ ->  softline <> ppr attrs
        <> case inits of
             [] -> empty
             _ ->  commasep (map ppr inits)

    ppr (TypedefGroup spec attrs typedefs _) =
        text "typedef" <+> ppr spec
        <> case attrs of
             [] -> empty
             _ ->  softline <> ppr attrs
        <> case typedefs of
             [] -> empty
             _ ->  commasep (map ppr typedefs)

    ppr (AntiDecls v _)  = pprAnti "decls" v
    ppr (AntiDecl v _)   = pprAnti "decl" v

instance Pretty Field where
    ppr (Field maybe_ident maybe_decl maybe_e _) =
        case maybe_decl of
          Nothing ->   empty
          Just decl -> pprDeclarator maybe_ident decl
        <>  case maybe_e of
              Nothing -> empty
              Just e ->  space <> colon <+> ppr e

instance Pretty FieldGroup where
    ppr (FieldGroup spec fields _) =
        ppr spec <> commasep (map ppr fields)

    ppr (AntiSdecls v _)  = pprAnti "sdecls" v
    ppr (AntiSdecl v _)   = pprAnti "sdecl" v

instance Pretty CEnum where
    ppr (CEnum ident maybe_e _) =
        ppr ident
        <> case maybe_e of
             Nothing -> empty
             Just e ->  space <> text "=" <+/> ppr e

    ppr (AntiEnums v _)  = pprAnti "enums" v
    ppr (AntiEnum v _)   = pprAnti "enum" v

instance Pretty Attr where
    ppr (Attr ident [] _) = ppr ident
    ppr (Attr ident args _) =
        ppr ident <> parens (commasep (map ppr args))

    pprList []    = empty
    pprList attrs = text "__attribute__" <>
                    parens (parens (commasep (map ppr attrs)))

instance Pretty Param where
    ppr (Param maybe_ident spec decl _) =
        ppr spec <> pprDeclarator maybe_ident decl

    ppr (AntiParams v _)  = pprAnti "params" v
    ppr (AntiParam v _)   = pprAnti "param" v

instance Pretty Params where
    ppr (Params args True _) =
        commasep (map ppr args ++ [text "..."])

    ppr (Params args False _) =
        commasep (map ppr args)

instance Pretty Func where
    ppr (Func spec ident decl args body loc) =
        ppr spec <> pprDeclarator (Just ident) (Proto decl args loc)
        </> ppr body

    ppr (OldFunc spec ident decl args maybe_initgroups body loc) =
        ppr spec <> pprDeclarator (Just ident) (OldProto decl args loc)
        </> case maybe_initgroups of
              Nothing -> empty
              Just initgroups ->
                  stack (zipWith (<>) (map ppr initgroups) (repeat semi))
        </> ppr body

instance Pretty Definition where
    ppr (FuncDef func loc)      = srcloc loc <> ppr func
    ppr (DecDef initgroup loc)  = srcloc loc <> ppr initgroup <> semi
    ppr (EscDef s loc)          = srcloc loc <> text s
    ppr (ObjCClassDec clss loc) = srcloc loc <> text "@class" <+> commasep (map ppr clss) <> semi
    ppr (ObjCClassIface cident sident refs ivars decls attrs loc)
      = srcloc loc
        <> case attrs of
             [] -> empty
             _  ->  ppr attrs <> softline
        <> text "@interface" <+> ppr cident <+> maybe empty (\ident -> char ':' <+> ppr ident) sident
        <+> pprIfaceBody refs ivars decls
    ppr (ObjCCatIface cident catident refs ivars decls loc)
      = srcloc loc
        <> text "@interface" <+> ppr cident <+> parens (maybe empty ppr catident) <+> pprIfaceBody refs ivars decls
    ppr (ObjCProtDec prots loc) = srcloc loc <> text "@protocol" <+> commasep (map ppr prots) <> semi
    ppr (ObjCProtDef pident refs decls loc)
      = srcloc loc
        <> text "@protocol" <+> ppr pident <+> pprIfaceBody refs [] decls
    ppr (ObjCClassImpl cident sident ivars defs loc)
      = srcloc loc
        <>   text "@implementation" <+> ppr cident <+> maybe empty (\ident -> char ':' <+> ppr ident) sident
        </>  stack (map ppr ivars)
        <//> stack (map ppr defs)
        </>  text "@end"
    ppr (ObjCCatImpl cident catident defs loc)
      = srcloc loc
        <>   text "@implementation" <+> ppr cident <+> parens (ppr catident)
        <//> stack (map ppr defs)
        </>  text "@end"
    ppr (ObjCSynDef pivars loc)
      = srcloc loc
        <> text "@synthesize" <+> commasep (map pprPivar pivars) <> semi
      where
        pprPivar (ident,  Nothing)     = ppr ident
        pprPivar (ident1, Just ident2) = ppr ident1 <> char '=' <> ppr ident2
    ppr (ObjCDynDef pivars loc)
      = srcloc loc
        <> text "@dynamic" <+> commasep (map ppr pivars) <> semi
    ppr (ObjCMethDef proto body loc)
      = srcloc loc
        <> ppr proto </> ppr body
    ppr (ObjCCompAlias aident cident loc)
      = srcloc loc
        <> text "@compatibility_alias" <+> ppr aident <+> ppr cident

    ppr (AntiFunc v _)    = pprAnti "func" v
    ppr (AntiEsc v _)     = pprAnti "esc" v
    ppr (AntiEdecls v _)  = pprAnti "edecls" v
    ppr (AntiEdecl v _)   = pprAnti "edecl" v

    pprList ds = stack (map ppr ds) <> line

pprIfaceBody :: [Id] -> [ObjCIvarDecl] -> [ObjCIfaceDecl] -> Doc
pprIfaceBody refs ivars decls
  = case refs of
      [] -> empty
      _  -> angleList (map ppr refs)
    </>  stack (map ppr ivars)
    <//> stack (map ppr decls)
    </>  text "@end"

instance Pretty ObjCIvarDecl where
    ppr (ObjCIvarVisi visi  loc) = pprLoc loc $ ppr visi
    ppr (ObjCIvarDecl field loc) = pprLoc loc $ ppr field <> semi

instance Pretty ObjCVisibilitySpec where
    ppr (ObjCPrivate _loc)   = text "@private"
    ppr (ObjCPublic _loc)    = text "@public"
    ppr (ObjCProtected _loc) = text "@protected"
    ppr (ObjCPackage _loc)   = text "@package"

instance Pretty ObjCIfaceDecl where
    ppr (ObjCIfaceProp attrs field loc)
      = pprLoc loc $
        text "@property"
        <+> case attrs of
              [] -> empty
              _  -> parensList (map ppr attrs) <> space
        <> ppr field
        <> semi
    ppr (ObjCIfaceReq req loc)
      = pprLoc loc $ ppr req
    ppr (ObjCIfaceMeth proto _loc)
      = ppr proto
        <> semi
    ppr (ObjCIfaceDecl decl loc)
      = pprLoc loc $ ppr decl

instance Pretty ObjCPropAttr where
    ppr (ObjCGetter ident loc)   = pprLoc loc $ text "getter=" <> ppr ident
    ppr (ObjCSetter ident loc)   = pprLoc loc $ text "setter=" <> ppr ident <> colon
    ppr (ObjCReadonly loc)       = pprLoc loc $ text "readonly"
    ppr (ObjCReadwrite loc)      = pprLoc loc $ text "readwrite"
    ppr (ObjCAssign loc)         = pprLoc loc $ text "assign"
    ppr (ObjCRetain loc)         = pprLoc loc $ text "retain"
    ppr (ObjCCopy loc)           = pprLoc loc $ text "copy"
    ppr (ObjCNonatomic loc)      = pprLoc loc $ text "nonatomic"
    ppr (ObjCAtomic loc)         = pprLoc loc $ text "atomic"
    ppr (ObjCStrong loc)         = pprLoc loc $ text "strong"
    ppr (ObjCWeak loc)           = pprLoc loc $ text "weak"
    ppr (ObjCUnsafeRetained loc) = pprLoc loc $ text "unsafe_retained"

instance Pretty ObjCMethodReq where
    ppr (ObjCRequired _loc) = text "@required"
    ppr (ObjCOptional _loc) = text "@optional"

instance Pretty ObjCParam where
    ppr (ObjCParam sel ty attrs arg loc)
      = pprLoc loc $
        case (sel, arg) of
         (Nothing , Nothing) -> error $ "pretty printing 'ObjCParam': empty " ++ show loc
         (Just sid, Nothing) -> ppr sid
         (_       , Just pid)
           -> maybe empty ppr sel <> colon <> maybe empty (parens . ppr) ty <> ppr attrs <> ppr pid

instance Pretty ObjCMethodProto where
    ppr (ObjCMethodProto isClassMeth resTy attrs1 params vargs attrs2 loc)
      = pprLoc loc $
        (if isClassMeth then char '+' else char '-')
        <+> maybe empty (parens . ppr) resTy
        <> case attrs1 of
             [] -> empty
             _  -> space <> ppr attrs1 <> space
        <> spread (map ppr params)
        <> if vargs then text ", ..." else empty
        <> ppr attrs2

instance Pretty Stm where
    ppr (Label ident stm sloc) =
        srcloc sloc <>
        indent (-2) (line <> ppr ident <> colon) </> ppr stm

    ppr (Case e stm sloc) =
        srcloc sloc <>
        line <>
        indent (-2) (line <> text "case" <+> ppr e <> colon) </> ppr stm

    ppr (Default stm sloc) =
        srcloc sloc <>
        indent (-2) (line <> text "default" <> colon) </> ppr stm

    ppr (Exp Nothing sloc) =
        srcloc sloc <> semi

    ppr (Exp (Just e) sloc) =
        srcloc sloc <> hang 4 (ppr e) <> semi

    ppr (Block items sloc) =
        srcloc sloc <> ppr items

    ppr (If test then' maybe_else sloc) =
        srcloc sloc <>
        text "if" <+> parens (ppr test)
        <> pprStm then'
        <> case maybe_else of
             Nothing     -> empty
             Just else'  -> space <> text "else" <> pprStm else'
      where
        pprStm :: Stm -> Doc
        pprStm stm@(Block _ _)   = space <> ppr stm
        pprStm stm@(If _ _ _ _)  = space <> ppr stm
        pprStm stm               = nest 4 (line <> ppr stm) <> line

    ppr (Switch e stm sloc) =
        srcloc sloc <>
        text "switch" <+> parens (ppr e ) <+/> ppr stm

    ppr (While e stm sloc) =
        srcloc sloc <>
        text "while" <+> parens (ppr e) <+/> ppr stm

    ppr (DoWhile stm e sloc) =
        srcloc sloc <>
        text "do" <+/> ppr stm <+/> text "while" <> parens(ppr e) <> semi

    ppr (For ini test post stm sloc) =
        srcloc sloc <>
        text "for"
        <+> (parens . semisep) [either ppr ppr ini, ppr test, ppr post]
        <> case stm of
             Block {} -> space <> ppr stm
             _ -> nest 4 $ line <> ppr stm

    ppr (Goto ident sloc) =
        srcloc sloc <>
        text "goto" <+> ppr ident <> semi

    ppr (Continue sloc) =
        srcloc sloc <> text "continue" <>semi

    ppr (Break sloc) =
        srcloc sloc <> text "break" <> semi

    ppr (Return Nothing sloc) =
        srcloc sloc <> text "return" <> semi

    ppr (Return (Just e) sloc) =
        srcloc sloc <> nest 4 (text "return" <+> ppr e) <> semi

    ppr (Pragma pragma sloc) =
        srcloc sloc <> text "#pragma" <+> text pragma

    ppr (Asm isVolatile _ template outputs inputs clobbered sloc) =
        srcloc sloc <>
        text "__asm__"
        <> case isVolatile of
             True ->  space <> text "__volatile__"
             False -> empty
        <> parens (pprAsm inputs clobbered)
        <> semi
      where
        pprAsm :: [(String, Exp)] -> [String] -> Doc
        pprAsm [] [] =
            spread (map text template)
            <> case outputs of
                 [] -> space <> colon
                 _ ->  colon <+/> commasep (map pprReg outputs)

        pprAsm inp clob =
            spread (map text template)
            <> case outputs of
                 [] -> space <> colon
                 _ ->  colon <+/> commasep (map pprReg outputs)
            <> case inp of
                 [] -> space <> colon
                 _ ->  colon <+/> commasep (map pprReg inputs)
            <> case clob of
                 [] -> space <> colon
                 _ ->  colon <+/> commasep (map text clobbered)

        pprReg :: (String, Exp) -> Doc
        pprReg (reg, e) = text reg <+> parens (ppr e)

    ppr (ObjCTry try catchs finally sloc) =
        srcloc sloc
        <>  text "@try"
        </> ppr try
        </> stack (map ppr catchs)
        </> case finally of
              Nothing    -> empty
              Just block -> text "@finally" </> ppr block

    ppr (ObjCThrow e sloc) =
        srcloc sloc
        <> text "@throw"
        <> case e of
             Nothing -> semi
             Just e' -> space <> ppr e' <> semi

    ppr (ObjCSynchronized e block sloc) =
        srcloc sloc
        <>  text "@synchronized" <+> parens (ppr e)
        </> ppr block

    ppr (ObjCAutoreleasepool block sloc) =
        srcloc sloc
        <>  text "@autoreleasepool"
        </> ppr block

    ppr (AntiPragma v _) = pprAnti "pragma" v
    ppr (AntiStm v _)    = pprAnti "stm" v
    ppr (AntiStms v _)   = pprAnti "stms" v

instance Pretty ObjCCatch where
    ppr (ObjCCatch Nothing     block loc) = srcloc loc <> text "@catch (...)" <+> ppr block
    ppr (ObjCCatch (Just param) block loc) = srcloc loc
                                            <> text "@catch" <+> parens (ppr param) <+> ppr block

    pprList = stack . map ppr

instance Pretty BlockItem where
    ppr (BlockDecl decl) = ppr decl <> semi
    ppr (BlockStm stm)   = ppr stm

    ppr (AntiBlockItem v _)  = pprAnti "item" v
    ppr (AntiBlockItems v _) = pprAnti "items" v

    pprList = embrace . loop
      where
        loop :: [BlockItem] -> [Doc]
        loop [] =
            []
        loop [item] =
            [ppr item]
        loop (item1@(BlockDecl _) : item2@(BlockStm _) : items) =
            (ppr item1 <> line) : loop (item2 : items)
        loop (item1@(BlockStm _) : item2@(BlockDecl _) : items) =
            (ppr item1 <> line) : loop (item2 : items)
        loop (item : items) =
            ppr item : loop items

        embrace :: [Doc] -> Doc
        embrace [] = lbrace <+> rbrace
        embrace ds = lbrace <>
                     nest 4 (line <> stack ds) </>
                     rbrace

instance Pretty Const where
    ppr (IntConst s _ _ _)          = text s
    ppr (LongIntConst s _ _ _)      = text s
    ppr (LongLongIntConst s _ _ _)  = text s
    ppr (FloatConst s _ _)          = text s
    ppr (DoubleConst s _ _)         = text s
    ppr (LongDoubleConst s _ _)     = text s
    ppr (CharConst s _ _)           = text s
    ppr (StringConst ss _ _)        = sep (map string ss)

    ppr (AntiConst v _)       = pprAnti "const"  v
    ppr (AntiString v _)      = pprAnti "string"  v
    ppr (AntiChar v _)        = pprAnti "char"    v
    ppr (AntiLongDouble v _)  = pprAnti "ldouble" v
    ppr (AntiDouble v _)      = pprAnti "double"  v
    ppr (AntiFloat v _)       = pprAnti "float"   v
    ppr (AntiULInt v _)       = pprAnti "ulint"   v
    ppr (AntiLInt v _)        = pprAnti "lint"    v
    ppr (AntiULLInt v _)      = pprAnti "ullint"  v
    ppr (AntiLLInt v _)       = pprAnti "llint"   v
    ppr (AntiUInt v _)        = pprAnti "uint"    v
    ppr (AntiInt v _)         = pprAnti "int"     v

instance Pretty Exp where
    pprPrec _ (Var ident loc) = pprLoc loc $ ppr ident
    pprPrec _ (Const k loc) = pprLoc loc $ ppr k

    pprPrec p (BinOp op e1 e2 loc) =
        pprLoc loc $
        infixop p op e1 e2

    pprPrec p (Assign e1 op e2 loc) =
        pprLoc loc $
        infixop p op e1 e2

    pprPrec p (PreInc e loc) =
        pprLoc loc $
        parensIf (p > 14) $
        text "++" <> pprPrec 14 e

    pprPrec p (PostInc e loc) =
        pprLoc loc $
        parensIf (p > 15) $
        pprPrec 15 e <> text "++"

    pprPrec p (PreDec e loc) =
        pprLoc loc $
        parensIf (p > 14) $
        text "--" <> pprPrec 14 e

    pprPrec p (PostDec e loc) =
        pprLoc loc $
        parensIf (p > 15) $
        pprPrec 15 e <> text "--"

    pprPrec p (UnOp op e loc) =
        pprLoc loc $
        parensIf (p > 14) $
        ppr op <> pprPrec 14 e

    pprPrec p (SizeofExp e loc) =
        pprLoc loc $
        parensIf (p > 14) $
        text "sizeof" <> parens (pprPrec 14 e)

    pprPrec p (SizeofType tipe loc) =
        pprLoc loc $
        parensIf (p > 14) $
        text "sizeof" <> parens (ppr tipe)

    pprPrec p (Cast tipe e loc) =
        pprLoc loc $
        parensIf (p > 14) $
        parens (ppr tipe) <+> pprPrec 14 e

    pprPrec p (Cond test then' else' loc) =
        pprLoc loc $
        parensIf (p > 3) $
        pprPrec 3 test <+> text "?" <+>
        pprPrec 3 then' <+> colon <+> pprPrec 3 else'

    pprPrec p (Member e ident loc) =
        pprLoc loc $
        parensIf (p > 15) $
        pprPrec 15 e <> dot <> ppr ident

    pprPrec p (PtrMember e ident loc) =
        pprLoc loc $
        parensIf (p > 15) $
        pprPrec 15 e <> text "->" <> ppr ident

    pprPrec p (Index e1 e2 loc) =
        pprLoc loc $
        parensIf (p > 15) $
        pprPrec 15 e1 <> brackets (ppr e2)

    pprPrec p (FnCall f args loc) =
        pprLoc loc $
        parensIf (p > 15) $
        pprPrec 15 f <> parensList (map ppr args)

    pprPrec p (CudaCall f config args loc) =
        pprLoc loc $
        parensIf (p > 15) $
        pprPrec 15 f <>
        text "<<<" <> pprConfig config <> text ">>>" <>
        parensList (map ppr args)
      where
        pprConfig :: ExeConfig -> Doc
        pprConfig conf = commasep $
            [ppr (exeGridDim conf), ppr (exeBlockDim conf)] ++
            (case exeSharedSize conf of
               Nothing -> []
               Just e -> [ppr e])
            ++
            (case exeStream conf of
               Nothing -> []
               Just e -> [ppr e])

    pprPrec p (Seq e1 e2 loc) =
        pprLoc loc $
        parensIf (p > 1) $
        pprPrec 1 e1 <> comma <+/> pprPrec 1 e2

    pprPrec p (CompoundLit ty inits loc) =
        pprLoc loc $
        parensIf (p > 15) $
        parens (ppr ty) <+>
        braces (commasep (map pprInit inits))
      where
        pprInit :: (Maybe Designation, Initializer) -> Doc
        pprInit (Nothing, ini) = ppr ini
        pprInit (Just d, ini)  = ppr d <+> text "=" <+/> ppr ini

    pprPrec _ (StmExpr blockItems loc) =
        pprLoc loc $ parens $
        ppr blockItems

    pprPrec _ (BuiltinVaArg e ty loc) =
        pprLoc loc $
        text "__builtin_va_arg(" <> ppr e <> comma <+> ppr ty <> rparen

    pprPrec _ (BlockLit ty attrs block loc) =
        pprLoc loc $
        char '^' <> ppr ty <>
        (if null attrs then empty else softline <> ppr attrs) <+>
        ppr block

    pprPrec _ (ObjCMsg recv args varArgs loc1) =
        pprLoc loc1 $
        brackets $
        ppr recv <+/>
        nest 2 (pprMsgArgs args)
      where
        pprMsgArgs ([ObjCArg (Just sel) Nothing loc]) = pprLoc loc $ ppr sel
        pprMsgArgs _                                  = sep (map pprMsgArg args) <>
                                                        cat (map pprVarArg varArgs)

        pprMsgArg (ObjCArg (Just sel) (Just e) loc) = pprLoc loc $ ppr sel <> colon <+> ppr e
        pprMsgArg (ObjCArg Nothing    (Just e) loc) = pprLoc loc $ colon <+> ppr e
        pprMsgArg (ObjCArg _          Nothing  loc)
          = error $ "pretty printing 'ObjCArg': missing expression at " ++ show loc

        pprVarArg e = comma <+> ppr e

    pprPrec _ (ObjCLitConst op c loc) =
        srcloc loc <>
        char '@' <>
        maybe empty ppr op <>
        ppr c

    pprPrec _ (ObjCLitString strs loc) =
        srcloc loc <>
        spread (map ((char '@' <>) . ppr) strs)

    pprPrec _ (ObjCLitBool False loc) =
        srcloc loc <>
        text "@NO"

    pprPrec _ (ObjCLitBool True loc) =
        srcloc loc <>
        text "@YES"

    pprPrec _ (ObjCLitArray es loc) =
        srcloc loc <>
        char '@' <> brackets
          (commasep (map ppr es))

    pprPrec _ (ObjCLitDict as loc) =
        srcloc loc <>
        char '@' <> braces
          (commasep (map (\(l, r) -> ppr l <+> colon <+> ppr r) as))

    pprPrec _ (ObjCLitBoxed e loc) =
        srcloc loc <>
        char '@' <> parens (ppr e)

    pprPrec _ (ObjCEncode t loc) =
        srcloc loc <>
        text "@encode" <> parens (ppr t)

    pprPrec _ (ObjCProtocol ident loc) =
        srcloc loc <>
        text "@protocol" <> parens (ppr ident)

    pprPrec _ (ObjCSelector sel loc) =
        srcloc loc <>
        text "@selector" <> parens (text sel)

    pprPrec _ (AntiArgs v _)  = pprAnti "args"  v

    pprPrec _ (AntiExp v _)   = pprAnti "var"  v

instance Pretty BinOp where
    ppr Add  = text "+"
    ppr Sub  = text "-"
    ppr Mul  = text "*"
    ppr Div  = text "/"
    ppr Mod  = text "%"
    ppr Eq   = text "=="
    ppr Ne   = text "!="
    ppr Lt   = text "<"
    ppr Gt   = text ">"
    ppr Le   = text "<="
    ppr Ge   = text ">="
    ppr Land = text "&&"
    ppr Lor  = text "||"
    ppr And  = text "&"
    ppr Or   = text "|"
    ppr Xor  = text "^"
    ppr Lsh  = text "<<"
    ppr Rsh  = text ">>"

instance Pretty AssignOp where
    ppr JustAssign = text "="
    ppr AddAssign  = text "+="
    ppr SubAssign  = text "-="
    ppr MulAssign  = text "*="
    ppr DivAssign  = text "/="
    ppr ModAssign  = text "%="
    ppr LshAssign  = text "<<="
    ppr RshAssign  = text ">>="
    ppr AndAssign  = text "&="
    ppr XorAssign  = text "^="
    ppr OrAssign   = text "|="

instance Pretty UnOp where
    ppr AddrOf   = text "&"
    ppr Deref    = text "*"
    ppr Positive = text "+"
    ppr Negate   = text "-"
    ppr Not      = text "~"
    ppr Lnot     = text "!"

instance Pretty BlockType where
    ppr (BlockVoid _loc)        = empty
    ppr (BlockParam params loc) = pprLoc loc $ parens (commasep (map ppr params))
    ppr (BlockType ty loc)      = pprLoc loc $ ppr ty

instance Pretty ObjCRecv where
    ppr (ObjCRecvSuper loc)               = pprLoc loc $ text "super"
    ppr (ObjCRecvExp e loc)               = pprLoc loc $ ppr e
    ppr (ObjCRecvClassName className loc) = pprLoc loc $ ppr className
    ppr (ObjCRecvTypeName typeName loc)   = pprLoc loc $ ppr typeName
