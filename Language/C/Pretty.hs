{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      :  Language.C.Pretty
-- Copyright   :  (c) 2006-2011 Harvard University
--                (c) 2011-2013 Geoffrey Mainland
--             :  (c) 2013-2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Language.C.Pretty where

import Data.Char (isAlphaNum,
                  isLower)
import Data.Loc
import Data.Maybe (isJust)
#if !(MIN_VERSION_base(4,9,0))
import Data.Monoid (Monoid(..), (<>))
#endif /* !(MIN_VERSION_base(4,9,0)) */
#if MIN_VERSION_base(4,9,0) && !(MIN_VERSION_base(4,11,0))
import Data.Semigroup (Semigroup(..))
#endif

import Language.C.Syntax
import Text.PrettyPrint.Mainland
import Text.PrettyPrint.Mainland.Class

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

-- | Pretty print infix binary operators
infixop :: (Pretty a, Pretty b, Pretty op, CFixity op)
        => Int -- ^ precedence of context
        -> op  -- ^ operator
        -> a   -- ^ left argument
        -> b   -- ^ right argument
        -> Doc
infixop prec op l r =
    parensOp prec op $
    pprPrec leftPrec l <+> ppr op <+/> pprPrec rightPrec r
  where
    leftPrec | opAssoc == RightAssoc = opPrec + 1
             | otherwise             = opPrec

    rightPrec | opAssoc == LeftAssoc = opPrec + 1
              | otherwise            = opPrec

    Fixity opAssoc opPrec = fixity op

-- | Pretty print prefix unary operators
prefixop :: (Pretty a, Pretty op, CFixity op)
         => Int -- ^ precedence of context
         -> op  -- ^ operator
         -> a   -- ^ argument
         -> Doc
prefixop prec op arg =
    parensIf (prec > opPrec) $
    ppr op <> pprPrec rightPrec arg
  where
    rightPrec | opAssoc == LeftAssoc = opPrec + 1
              | otherwise            = opPrec

    Fixity opAssoc opPrec = fixity op

parensList :: [Doc] -> Doc
parensList = enclosesep lparen rparen comma

bracesList :: [Doc] -> Doc
bracesList = enclosesep lbrace rbrace comma

bracesSemiList :: [Doc] -> Doc
bracesSemiList = enclosesep lbrace rbrace semi

angleList :: [Doc] -> Doc
angleList = enclosesep langle rangle comma

embrace :: [Doc] -> Doc
embrace [] = lbrace <+> rbrace
embrace ds = lbrace <>
             nest 4 (line <> stack ds) </>
             rbrace

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

    parensOp :: Int -> a -> Doc -> Doc
    parensOp prec op =
        parensIf (prec > opPrec)
      where
        Fixity _ opPrec = fixity op

--
-- Fixities are taken from Table 2-1 in Section 2.12 of K&R (2nd ed.)
--
commaPrec :: Int
commaPrec = 1

commaPrec1 :: Int
commaPrec1 = commaPrec + 1

condPrec :: Int
condPrec = 3

condPrec1 :: Int
condPrec1 = condPrec + 1

unopPrec :: Int
unopPrec = 14

unopPrec1 :: Int
unopPrec1 = unopPrec + 1

memberPrec :: Int
memberPrec = 15

memberPrec1 :: Int
memberPrec1 = memberPrec + 1

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

    parensOp prec op =
        go op
      where
        go :: BinOp -> Doc -> Doc
        go Add  | isBitwiseOp = parens
        go Sub  | isBitwiseOp = parens
        go Land | isOp Lor    = parens
        go Lor  | isOp Land   = parens
        go And  | isOp Or     = parens
                | isOp Xor    = parens
        go Or   | isOp And    = parens
                | isOp Xor    = parens
        go Xor  | isOp And    = parens
                | isOp Or     = parens
        go _                  = parensIf (prec > opPrec)

        isBitwiseOp :: Bool
        isBitwiseOp = isOp And || isOp Or || isOp Xor

        -- Return 'True' if we are potentially an immediate subterm of the
        -- binary operator op'. We make this determination based of the value of
        -- @prec@.
        isOp :: BinOp -> Bool
        isOp op' = prec == op'Prec || prec == op'Prec + 1
          where
            Fixity _ op'Prec = fixity op'

        Fixity _ opPrec = fixity op

instance CFixity AssignOp where
    fixity _ = infixr_ 2

instance CFixity UnOp where
    fixity _ = infixr_ unopPrec

instance Pretty Id where
    ppr (Id ident _)  = text ident
    ppr (AntiId v _)  = pprAnti "id" v

instance Pretty StringLit where
    ppr (StringLit ss _ _) = sep (map string ss)

instance Pretty Storage where
    ppr (Tauto _)                    = text "auto"
    ppr (Tregister _)                = text "register"
    ppr (Tstatic _)                  = text "static"
    ppr (Textern Nothing _)          = text "extern"
    ppr (Textern (Just l) _)         = text "extern" <+> ppr l
    ppr (Ttypedef _)                 = text "typedef"
    ppr (T__block _)                 = text "__block"
    ppr (TObjC__weak _)              = text "__weak"
    ppr (TObjC__strong _)            = text "__strong"
    ppr (TObjC__unsafe_unretained _) = text "__unsafe_unretained"

instance Pretty TypeQual where
    ppr (Tconst _)          = text "const"
    ppr (Tvolatile _)       = text "volatile"

    ppr (EscTypeQual esc _) = text esc

    ppr (AntiTypeQual v _)  = pprAnti "tyqual" v
    ppr (AntiTypeQuals v _) = pprAnti "tyquals" v

    ppr (Tinline _)         = text "inline"
    ppr (Trestrict _)       = text "restrict"

    ppr (TAttr attr)        = ppr [attr]
    ppr (T__restrict _)     = text "__restrict"

    ppr (TCUDAdevice _)     = text "__device__"
    ppr (TCUDAglobal _)     = text "__global__"
    ppr (TCUDAhost _)       = text "__host__"
    ppr (TCUDAconstant _)   = text "__constant__"
    ppr (TCUDAshared _)     = text "__shared__"
    ppr (TCUDArestrict _)   = text "__restrict__"
    ppr (TCUDAnoinline _)   = text "__noinline__"

    ppr (TCLprivate _)      = text "__private"
    ppr (TCLlocal _)        = text "__local"
    ppr (TCLglobal _)       = text "__global"
    ppr (TCLconstant _)     = text "__constant"
    ppr (TCLreadonly _)     = text "read_only"
    ppr (TCLwriteonly _)    = text "write_only"
    ppr (TCLkernel _)       = text "__kernel"

instance Pretty Sign where
    ppr (Tsigned _)    = text "signed"
    ppr (Tunsigned _)  = text "unsigned"

instance Pretty TypeSpec where
    ppr (Tvoid _)            = text "void"
    ppr (Tchar sign _)       = ppr sign <+> text "char"
    ppr (Tshort sign _)      = ppr sign <+> text "short"
    ppr (Tint sign _)        = ppr sign <+> text "int"
    ppr (Tlong sign _)       = ppr sign <+> text "long"
    ppr (Tlong_long sign _)  = ppr sign <+> text "long long"
    ppr (Tfloat _)           = text "float"
    ppr (Tdouble _)          = text "double"
    ppr (Tlong_double _)     = text "long double"

    ppr (Tstruct maybe_ident maybe_fields attrs _) =
        align $ pprStructOrUnion "struct" maybe_ident maybe_fields attrs

    ppr (Tunion maybe_ident maybe_fields attrs _) =
        align $ pprStructOrUnion "union" maybe_ident maybe_fields attrs

    ppr (Tenum maybe_ident cenums attrs _) =
        align $ pprEnum maybe_ident cenums attrs

    ppr (Tnamed ident refs _) =
        ppr ident <> if null refs then empty else angles (commasep (map ppr refs))

    ppr (T_Bool _) =
        text "_Bool"

    ppr (Tfloat_Complex _) =
        text "float" <+> text "_Complex"

    ppr (Tdouble_Complex _) =
        text "double" <+> text "_Complex"

    ppr (Tlong_double_Complex _) =
        text "long" <+> text "double" <+> text "_Complex"

    ppr (Tfloat_Imaginary _) =
        text "float" <+> text "_Imaginary"

    ppr (Tdouble_Imaginary _) =
        text "double" <+> text "_Imaginary"

    ppr (Tlong_double_Imaginary _) =
        text "long" <+> text "double" <+> text "_Imaginary"

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
    text ty <+> ppr maybe_ident <+> ppr maybe_fields <+/> ppr attrs

pprEnum :: Maybe Id
        -> [CEnum]
        -> [Attr]
        -> Doc
pprEnum maybe_ident cenums attrs =
    text "enum" <+> ppr maybe_ident <+> ppr cenums <+/> ppr attrs

instance Pretty DeclSpec where
    ppr (DeclSpec storage quals spec _) =
        case map ppr storage ++ map ppr quals of
          []   -> ppr spec
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
      Just ident -> pprDecl declarator (ppr ident)
    where
      pprPtr :: Decl -> Doc -> (Decl, Doc)
      pprPtr (Ptr quals decl _) post =
          pprPtr decl $
          text "*" <> spread (map ppr quals) <+> post

      pprPtr (BlockPtr quals decl _) post =
          pprPtr decl $
          text "^" <> spread (map ppr quals) <+> post

      pprPtr decl post =
          (decl, post)

      pprDirDecl :: Decl -> Doc -> (Decl, Doc)
      pprDirDecl (Array quals size decl _) pre =
          pprDirDecl decl $
          pre <> brackets (align (spread (map ppr quals) <+> ppr size))

      pprDirDecl (Proto decl args _) pre =
          pprDirDecl decl $
          pre <> parens (ppr args)

      pprDirDecl (OldProto decl args _) pre =
          pprDirDecl decl $
          pre <> parensList (map ppr args)

      pprDirDecl decl pre =
          (decl, pre)

      pprDecl :: Decl -> Doc -> Doc
      pprDecl decl mid =
          case decl' of
            DeclRoot {}     -> declDoc
            AntiTypeDecl {} -> declDoc
            _               -> pprDecl decl' (parens declDoc)
        where
          (decl', declDoc) = uncurry pprPtr (pprDirDecl decl mid)

instance Pretty Type where
    ppr (Type spec decl _)  = ppr spec <+> pprDeclarator Nothing decl
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
        pprDeclarator (Just ident) decl <+/> ppr attrs
        <+> case maybe_asmlabel of
              Nothing -> empty
              Just l ->  text "asm" <+> parens (ppr l)
        <+> case maybe_e of
              Nothing -> empty
              Just e ->  text "=" <+/> ppr e

instance Pretty Typedef where
    ppr (Typedef ident decl attrs loc) =
        ppr (Init ident decl Nothing Nothing attrs loc)

instance Pretty InitGroup where
    ppr (InitGroup spec attrs inits _) =
        ppr spec <+/> ppr attrs <+> commasep (map ppr inits)

    ppr (TypedefGroup spec attrs typedefs _) =
        text "typedef" <+> ppr spec <+/> ppr attrs <+> commasep (map ppr typedefs)

    ppr (AntiDecls v _)  = pprAnti "decls" v
    ppr (AntiDecl v _)   = pprAnti "decl" v

    pprList initgroups =
        stack (zipWith (<>) (map ppr initgroups) (repeat semi))

instance Pretty Field where
    ppr (Field maybe_ident maybe_decl maybe_e _) =
        case maybe_decl of
          Nothing   -> empty
          Just decl -> pprDeclarator maybe_ident decl
        <+>
        case maybe_e of
          Nothing -> empty
          Just e  -> colon <+> ppr e

instance Pretty FieldGroup where
    ppr (FieldGroup spec fields _) =
        ppr spec <+> commasep (map ppr fields)

    ppr (AntiSdecls v _)  = pprAnti "sdecls" v
    ppr (AntiSdecl v _)   = pprAnti "sdecl" v

    pprList fields = embrace (zipWith (<>) (map ppr fields) (repeat semi))

instance Pretty CEnum where
    ppr (CEnum ident maybe_e _) =
        ppr ident <+>
        case maybe_e of
          Nothing -> empty
          Just e ->  text "=" <+/> ppr e

    ppr (AntiEnums v _)  = pprAnti "enums" v
    ppr (AntiEnum v _)   = pprAnti "enum" v

    pprList []     = empty
    pprList cenums = embrace (zipWith (<>) (map ppr cenums) (repeat comma))

instance Pretty Attr where
    ppr (Attr ident [] _) = ppr ident
    ppr (Attr ident args _) =
        ppr ident <> parens (commasep (map ppr args))

    ppr (AntiAttr v _)   = pprAnti "attr" v
    ppr (AntiAttrs v _)  = pprAnti "attrs" v

    pprList []    = empty
    pprList attrs = text "__attribute__" <>
                    parens (parens (commasep (map ppr attrs)))

instance Pretty Param where
    ppr (Param maybe_ident spec decl _) =
        ppr spec <+> pprDeclarator maybe_ident decl

    ppr (AntiParams v _)  = pprAnti "params" v
    ppr (AntiParam v _)   = pprAnti "param" v

instance Pretty Params where
    ppr (Params args True _) =
        commasep (map ppr args ++ [text "..."])

    ppr (Params args False _) =
        commasep (map ppr args)

instance Pretty Func where
    ppr (Func spec ident decl args body loc) =
        ppr spec <+> pprDeclarator (Just ident) (Proto decl args loc) </> ppr body

    ppr (OldFunc spec ident decl args maybe_initgroups body loc) =
        ppr spec <+> pprDeclarator (Just ident) (OldProto decl args loc) </>
        ppr maybe_initgroups </>
        ppr body

instance Pretty Definition where
    ppr (FuncDef func loc)      = srcloc loc <> ppr func
    ppr (DecDef initgroup loc)  = srcloc loc <> ppr initgroup <> semi
    ppr (EscDef s loc)          = srcloc loc <> text s
    ppr (ObjCClassDec clss loc) = srcloc loc <> text "@class" <+> commasep (map ppr clss) <> semi

    ppr (AntiFunc v _)    = pprAnti "func" v
    ppr (AntiEsc v _)     = pprAnti "esc" v
    ppr (AntiEdecls v _)  = pprAnti "edecls" v
    ppr (AntiEdecl v _)   = pprAnti "edecl" v

    ppr (ObjCClassIface cident sident refs ivars decls attrs loc) =
        srcloc loc <+> ppr attrs <+/>
        text "@interface" <+> ppr cident <+> maybe empty (\ident -> char ':' <+> ppr ident) sident <+>
        pprIfaceBody refs ivars decls

    ppr (ObjCCatIface cident catident refs ivars decls loc) =
        srcloc loc <>
        text "@interface" <+> ppr cident <+> parens (maybe empty ppr catident) <+> pprIfaceBody refs ivars decls

    ppr (ObjCProtDec prots loc) =
        srcloc loc <> text "@protocol" <+> commasep (map ppr prots) <> semi

    ppr (ObjCProtDef pident refs decls loc) =
        srcloc loc <>
        text "@protocol" <+> ppr pident <+> pprIfaceBody refs [] decls

    ppr (ObjCClassImpl cident sident ivars defs loc) =
        srcloc loc <>
        text "@implementation" <+> ppr cident <+> maybe empty (\ident -> char ':' <+> ppr ident) sident </>
        stack (map ppr ivars) <//>
        stack (map ppr defs) </>
        text "@end"

    ppr (ObjCCatImpl cident catident defs loc) =
        srcloc loc <>
        text "@implementation" <+> ppr cident <+> parens (ppr catident) <//>
        stack (map ppr defs) </>
        text "@end"

    ppr (ObjCSynDef pivars loc) =
        srcloc loc <>
        text "@synthesize" <+> commasep (map pprPivar pivars) <> semi
      where
        pprPivar (ident,  Nothing)     = ppr ident
        pprPivar (ident1, Just ident2) = ppr ident1 <> char '=' <> ppr ident2

    ppr (ObjCDynDef pivars loc) =
        srcloc loc <>
        text "@dynamic" <+> commasep (map ppr pivars) <> semi

    ppr (ObjCMethDef proto body loc) =
        srcloc loc <>
        ppr proto </> ppr body

    ppr (ObjCCompAlias aident cident loc) =
        srcloc loc <>
        text "@compatibility_alias" <+> ppr aident <+> ppr cident

    ppr (AntiObjCMeth v _)  = pprAnti "methdef" v
    ppr (AntiObjCMeths v _) = pprAnti "methdefs" v

    pprList ds = stack (map ppr ds) <> line

pprIfaceBody :: [Id] -> [ObjCIvarDecl] -> [ObjCIfaceDecl] -> Doc
pprIfaceBody refs ivars decls =
    case refs of
      [] -> empty
      _  -> angleList (map ppr refs)
    </>  stack (map ppr ivars)
    <//> stack (map ppr decls)
    </>  text "@end"

instance Pretty Stm where
    ppr (Label ident attrs stm sloc) =
        srcloc sloc <>
        indent (-2) (line <> ppr ident <> colon <+> ppr attrs) </> ppr stm

    ppr (Case e stm sloc) =
        srcloc sloc <>
        indent (-2) (line <> text "case" <+> ppr e <> colon) </> ppr stm

    ppr (CaseRange e1 e2 stm sloc) =
        srcloc sloc <>
        indent (-2) (line <> text "case" <+> ppr e1 <+> text "..." <+> ppr e2 <> colon) </> ppr stm

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
        text "if" <+> parens (ppr test) <>
        pprThen then' (fmap pprElse maybe_else)
      where
        isIf :: Stm -> Bool
        isIf If{} = True
        isIf (Comment _ stm _) = isIf stm
        isIf _ = False

        pprThen :: Stm -> Maybe Doc -> Doc
        pprThen stm@(Block {}) rest        = space <> ppr stm <+> maybe empty id rest
        pprThen stm            rest
          | isIf stm                       = space <> ppr [BlockStm stm] <+> maybe empty id rest
        pprThen stm            Nothing     = nest 4 (line <> ppr stm)
        pprThen stm            (Just rest) = nest 4 (line <> ppr stm) </> rest

        pprElse :: Stm -> Doc
        pprElse stm =
            text "else" <> go stm
          where
            go :: Stm -> Doc
            go (Block {}) = space <> ppr stm
            go (If {})    = space <> ppr stm
            go _stm       = nest 4 (line <> ppr stm)

    ppr (Switch e stm sloc) =
        srcloc sloc <>
        text "switch" <+> parens (ppr e) <> pprBlock stm

    ppr (While e stm sloc) =
        srcloc sloc <>
        text "while" <+> parens (ppr e) <> pprBlock stm

    ppr (DoWhile stm e sloc) =
        srcloc sloc <>
        text "do" <> pprBlock stm <+/> text "while" <> parens (ppr e) <> semi

    ppr (For ini test post stm sloc) =
        srcloc sloc <>
        text "for" <+>
        (parens . semisep) [either ppr ppr ini, ppr test, ppr post] <>
        pprBlock stm

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

    ppr (Comment com stm sloc) =
        align $ srcloc sloc <> text com </> ppr stm

    ppr (EscStm esc sloc) =
        srcloc sloc <> text esc

    ppr (AntiEscStm v _)      = pprAnti "escstm" v
    ppr (AntiPragma v _)      = pprAnti "pragma" v
    ppr (AntiComment v stm _) = pprAnti "pragma" v </> ppr stm
    ppr (AntiStm v _)         = pprAnti "stm" v
    ppr (AntiStms v _)        = pprAnti "stms" v

    ppr (Asm isVolatile _ template outs ins clobbered sloc) =
        srcloc sloc <>
        text "__asm__"
        <> case isVolatile of
             True ->  space <> text "__volatile__"
             False -> empty
        <> parens (ppr template
                   <> case outs of
                        [] -> space <> colon
                        _ ->  colon <+/> ppr outs
                   <> case ins of
                        [] -> space <> colon
                        _ ->  colon <+/> ppr ins
                   <> case clobbered of
                        [] -> space <> colon
                        _ ->  colon <+/> commasep (map text clobbered)
                  )
        <> semi

    ppr (AsmGoto isVolatile _ template ins clobbered labels sloc) =
        srcloc sloc <>
        text "__asm__"
        <> case isVolatile of
             True ->  space <> text "__volatile__"
             False -> empty
        <> parens (ppr template
                   <> colon
                   <> case ins of
                        [] -> space <> colon
                        _ ->  colon <+/> ppr ins
                   <> case clobbered of
                        [] -> space <> colon
                        _ ->  colon <+/> commasep (map text clobbered)
                   <> case clobbered of
                        [] -> space <> colon
                        _ ->  colon <+/> commasep (map ppr labels)
                  )
        <> semi

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

pprBlock :: Stm -> Doc
pprBlock stm@(Block {}) = space <> ppr stm
pprBlock stm@(If {})    = space <> ppr [BlockStm stm]
pprBlock stm            = nest 4 $ line <> ppr stm

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

instance Pretty Const where
    pprPrec p (IntConst s _ i _)          = parensIf (i < 0 && p > unopPrec) $
                                            text s
    pprPrec p (LongIntConst s _ i _)      = parensIf (i < 0 && p > unopPrec) $
                                            text s
    pprPrec p (LongLongIntConst s _ i _)  = parensIf (i < 0 && p > unopPrec) $
                                            text s
    pprPrec p (FloatConst s r _)          = parensIf (r < 0 && p > unopPrec) $
                                            text s
    pprPrec p (DoubleConst s r _)         = parensIf (r < 0 && p > unopPrec) $
                                            text s
    pprPrec p (LongDoubleConst s r _)     = parensIf (r < 0 && p > unopPrec) $
                                            text s
    pprPrec _ (CharConst s _ _)           = text s
    pprPrec _ (StringConst ss _ _)        = sep (map string ss)

    pprPrec _ (AntiConst v _)       = pprAnti "const"  v
    pprPrec _ (AntiString v _)      = pprAnti "string"  v
    pprPrec _ (AntiChar v _)        = pprAnti "char"    v
    pprPrec _ (AntiLongDouble v _)  = pprAnti "ldouble" v
    pprPrec _ (AntiDouble v _)      = pprAnti "double"  v
    pprPrec _ (AntiFloat v _)       = pprAnti "float"   v
    pprPrec _ (AntiULInt v _)       = pprAnti "ulint"   v
    pprPrec _ (AntiLInt v _)        = pprAnti "lint"    v
    pprPrec _ (AntiULLInt v _)      = pprAnti "ullint"  v
    pprPrec _ (AntiLLInt v _)       = pprAnti "llint"   v
    pprPrec _ (AntiUInt v _)        = pprAnti "uint"    v
    pprPrec _ (AntiInt v _)         = pprAnti "int"     v

instance Pretty Exp where
    pprPrec p (Var ident loc) =
        pprLoc loc $
        pprPrec p ident

    pprPrec p (Const k loc) =
        pprLoc loc $
        pprPrec p k

    pprPrec p (BinOp op e1 e2 loc) =
        pprLoc loc $
        infixop p op e1 e2

    pprPrec p (Assign e1 op e2 loc) =
        pprLoc loc $
        infixop p op e1 e2

    pprPrec p (PreInc e loc) =
        pprLoc loc $
        parensIf (p > unopPrec) $
        text "++" <> pprPrec unopPrec1 e

    pprPrec p (PostInc e loc) =
        pprLoc loc $
        parensIf (p > unopPrec) $
        pprPrec unopPrec1 e <> text "++"

    pprPrec p (PreDec e loc) =
        pprLoc loc $
        parensIf (p > unopPrec) $
        text "--" <> pprPrec unopPrec1 e

    pprPrec p (PostDec e loc) =
        pprLoc loc $
        parensIf (p > unopPrec) $
        pprPrec unopPrec1 e <> text "--"

    pprPrec _ (EscExp e loc) =
        srcloc loc <> text e

    pprPrec p (AntiEscExp e loc) =
        pprLoc loc $
        parensIf (p > unopPrec) $
        text e

    -- When printing leading + and - operators, we print the argument at
    -- precedence 'unopPrec1' to ensure we get parentheses in cases like
    -- @-(-42)@. The same holds for @++@ and @--@ above.
    pprPrec p (UnOp op@Positive e loc) =
        pprLoc loc $
        parensIf (p > unopPrec) $
        ppr op <> pprPrec unopPrec1 e

    pprPrec p (UnOp op@Negate e loc) =
        pprLoc loc $
        parensIf (p > unopPrec) $
        ppr op <> pprPrec unopPrec1 e

    pprPrec p (UnOp op e loc) =
        pprLoc loc $
        prefixop p op e

    pprPrec p (SizeofExp e loc) =
        pprLoc loc $
        parensIf (p > unopPrec) $
        text "sizeof" <> parens (ppr e)

    pprPrec p (SizeofType tipe loc) =
        pprLoc loc $
        parensIf (p > unopPrec) $
        text "sizeof" <> parens (ppr tipe)

    pprPrec p (Cast tipe e loc) =
        pprLoc loc $
        parensIf (p > unopPrec) $
        parens (ppr tipe) <+> pprPrec unopPrec e

    pprPrec p (Cond test then' else' loc) =
        pprLoc loc $
        parensIf (p > condPrec) $
        pprPrec condPrec1 test <+> text "?" <+>
        pprPrec condPrec1 then' <+> colon <+>
        pprPrec condPrec else'

    pprPrec p (Member e ident loc) =
        pprLoc loc $
        parensIf (p > memberPrec) $
        pprPrec memberPrec e <> dot <> ppr ident

    pprPrec p (PtrMember e ident loc) =
        pprLoc loc $
        parensIf (p > memberPrec) $
        pprPrec memberPrec e <> text "->" <> ppr ident

    pprPrec p (Index e1 e2 loc) =
        pprLoc loc $
        parensIf (p > memberPrec) $
        pprPrec memberPrec e1 <> brackets (ppr e2)

    pprPrec p (FnCall f args loc) =
        pprLoc loc $
        parensIf (p > memberPrec) $
        pprPrec memberPrec f <> parensList (map ppr args)

    pprPrec p (Seq e1 e2 loc) =
        pprLoc loc $
        parensIf (p > commaPrec) $
        pprPrec commaPrec e1 <> comma <+/> pprPrec commaPrec1 e2

    pprPrec p (CompoundLit ty inits loc) =
        pprLoc loc $
        parensIf (p > memberPrec) $
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

    pprPrec p (CudaCall f config args loc) =
        pprLoc loc $
        parensIf (p > memberPrec) $
        pprPrec memberPrec f <>
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
        pprMsgArg (AntiObjCArg v _)  = pprAnti "kwarg" v
        pprMsgArg (AntiObjCArgs v _) = pprAnti "kwargs" v

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
          (commasep (map ppr as))

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

    pprPrec _ (Lambda captureList decl blockItems loc) =
        srcloc loc <>
        ppr captureList <>
        ppr decl <>
        ppr blockItems

    pprPrec _ (AntiArgs v _)  = pprAnti "args"  v

    pprPrec _ (AntiExp v _)   = pprAnti "var"  v

instance Pretty LambdaDeclarator where
    pprPrec _ (LambdaDeclarator params isMutable returnType _) =
        parens (ppr params) <>
        (if isMutable then text "mutable" else empty) <>
        (if isJust returnType then text "->" <> ppr returnType else empty)

instance Pretty LambdaIntroducer where
    pprPrec _ (LambdaIntroducer items loc) = pprLoc loc $ brackets $ commasep (map ppr items)

instance Pretty CaptureListEntry where
    pprPrec _ DefaultByValue = char '='
    pprPrec _ DefaultByReference = char '&'

instance Pretty ObjCDictElem where
    pprPrec _ (ObjCDictElem l r _)    = ppr l <+> colon <+> ppr r
    pprPrec _ (AntiObjCDictElems v _) = pprAnti "dictelems" v

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

instance Pretty AsmOut where
    ppr (AsmOut Nothing constraint ident) =
        text constraint <+> parens (ppr ident)

    ppr (AsmOut (Just sym) constraint ident) =
        brackets (ppr sym) <+> text constraint <+> parens (ppr ident)

    pprList []   = empty
    pprList outs = commasep (map ppr outs)

instance Pretty AsmIn where
    ppr (AsmIn Nothing constraint e) =
        text constraint <+> parens (ppr e)

    ppr (AsmIn (Just sym) constraint e) =
        brackets (ppr sym) <+> text constraint <+> parens (ppr e)

    pprList []  = empty
    pprList ins = commasep (map ppr ins)

instance Pretty BlockType where
    ppr (BlockVoid _loc)        = empty
    ppr (BlockParam params loc) = pprLoc loc $ parens (commasep (map ppr params))
    ppr (BlockType ty loc)      = pprLoc loc $ ppr ty

instance Pretty ObjCIvarDecl where
    ppr (ObjCIvarVisi visi  loc) = pprLoc loc $ ppr visi
    ppr (ObjCIvarDecl field loc) = pprLoc loc $ ppr field <> semi

instance Pretty ObjCVisibilitySpec where
    ppr (ObjCPrivate _loc)   = text "@private"
    ppr (ObjCPublic _loc)    = text "@public"
    ppr (ObjCProtected _loc) = text "@protected"
    ppr (ObjCPackage _loc)   = text "@package"

instance Pretty ObjCIfaceDecl where
    ppr (ObjCIfaceProp attrs field loc) =
        pprLoc loc $
        text "@property"
        <+> case attrs of
              [] -> empty
              _  -> parensList (map ppr attrs) <> space
        <> ppr field
        <> semi
    ppr (ObjCIfaceReq req loc) =
        pprLoc loc $ ppr req
    ppr (ObjCIfaceMeth proto _loc) =
        ppr proto <> semi
    ppr (ObjCIfaceDecl decl loc) =
        pprLoc loc $ ppr decl
    ppr (AntiObjCIfaceDecl v _loc) =
        pprAnti "ifdecl" v
    ppr (AntiObjCIfaceDecls v _loc) =
        pprAnti "ifdecls" v

    ppr (AntiObjCProp v _)  = pprAnti "prop" v
    ppr (AntiObjCProps v _) = pprAnti "props" v

instance Pretty ObjCPropAttr where
    ppr (ObjCGetter ident loc)     = pprLoc loc $ text "getter=" <> ppr ident
    ppr (ObjCSetter ident loc)     = pprLoc loc $ text "setter=" <> ppr ident <> colon
    ppr (ObjCReadonly loc)         = pprLoc loc $ text "readonly"
    ppr (ObjCReadwrite loc)        = pprLoc loc $ text "readwrite"
    ppr (ObjCAssign loc)           = pprLoc loc $ text "assign"
    ppr (ObjCRetain loc)           = pprLoc loc $ text "retain"
    ppr (ObjCCopy loc)             = pprLoc loc $ text "copy"
    ppr (ObjCNonatomic loc)        = pprLoc loc $ text "nonatomic"
    ppr (ObjCAtomic loc)           = pprLoc loc $ text "atomic"
    ppr (ObjCStrong loc)           = pprLoc loc $ text "strong"
    ppr (ObjCWeak loc)             = pprLoc loc $ text "weak"
    ppr (ObjCUnsafeUnretained loc) = pprLoc loc $ text "unsafe_unretained"
    ppr (AntiObjCAttr v _)         = pprAnti "propattr" v
    ppr (AntiObjCAttrs v _)        = pprAnti "propattrs" v

instance Pretty ObjCMethodReq where
    ppr (ObjCRequired _loc) = text "@required"
    ppr (ObjCOptional _loc) = text "@optional"

instance Pretty ObjCParam where
    ppr (ObjCParam sel ty attrs arg loc) =
        pprLoc loc $
        case (sel, arg) of
         (Nothing , Nothing) -> error $ "pretty printing 'ObjCParam': empty " ++ show loc
         (Just sid, Nothing) -> ppr sid
         (_       , Just pid)
           -> maybe empty ppr sel <> colon <> maybe empty (parens . ppr) ty <> ppr attrs <> ppr pid
    ppr (AntiObjCParam p _)  = pprAnti "methparam" p
    ppr (AntiObjCParams v _) = pprAnti "methparams" v

instance Pretty ObjCMethodProto where
    ppr (ObjCMethodProto isClassMeth resTy attrs1 params vargs attrs2 loc) =
        pprLoc loc $
        (if isClassMeth then char '+' else char '-') <+>
        maybe empty (parens . ppr) resTy <+>
        ppr attrs1 <+>
        spread (map ppr params) <>
        (if vargs then text ", ..." else empty) <+>
        ppr attrs2
    ppr (AntiObjCMethodProto p _) = pprAnti "methproto" p

instance Pretty ObjCCatch where
    ppr (ObjCCatch Nothing     block loc)  =
        srcloc loc <>
        text "@catch (...)" <+> ppr block

    ppr (ObjCCatch (Just param) block loc) =
        srcloc loc <>
        text "@catch" <+> parens (ppr param) <+> ppr block

    pprList = stack . map ppr

instance Pretty ObjCRecv where
    ppr (ObjCRecvSuper loc) = pprLoc loc $ text "super"
    ppr (ObjCRecvExp e loc) = pprLoc loc $ ppr e
    ppr (AntiObjCRecv v _)  = pprAnti "recv" v
