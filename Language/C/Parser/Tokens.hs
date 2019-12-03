-- |
-- Module      :  Language.C.Parser.Tokens
-- Copyright   :  (c) 2006-2011 Harvard University
--                (c) 2011-2013 Geoffrey Mainland
--                (c) 2013 Manuel M T Chakravarty
--                (c) 2013-2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Language.C.Parser.Tokens (
    Token(..),
    ExtensionsInt,
    keywords,
    keywordMap
  ) where

import Data.Bits
import Data.Char (isAlphaNum,
                  isLower)
import Data.List (foldl')
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Word
import Text.PrettyPrint.Mainland
import Text.PrettyPrint.Mainland.Class

import Language.C.Syntax (Extensions(..),
                          Signed(..))

data Token = Teof
           | Tpragma String
           | Tcomment String -- ^ Raw comment string
           | TintConst (String, Signed, Integer)
           | TlongIntConst (String, Signed, Integer)
           | TlongLongIntConst (String, Signed, Integer)
           | TfloatConst (String, Float)
           | TdoubleConst (String, Double)
           | TlongDoubleConst (String, Double)
           | TcharConst (String, Char)
           | TstringConst (String, String)
           | Tidentifier String
           | Tnamed String
           | Tlparen
           | Trparen
           | Tlbrack
           | Trbrack
           | Tlbrace
           | Trbrace
           | Tcomma
           | Tsemi
           | Tcolon
           | Tquestion
           | Tdot
           | Tarrow
           | Tellipses

           | Tplus
           | Tminus
           | Tstar
           | Tdiv
           | Tmod
           | Tnot
           | Tand
           | Tor
           | Txor
           | Tlsh
           | Trsh
           | Tinc
           | Tdec

           | Tlnot
           | Tland
           | Tlor

           | Teq
           | Tne
           | Tlt
           | Tgt
           | Tle
           | Tge

           | Tassign
           | Tadd_assign
           | Tsub_assign
           | Tmul_assign
           | Tdiv_assign
           | Tmod_assign
           | Tlsh_assign
           | Trsh_assign
           | Tand_assign
           | Tor_assign
           | Txor_assign

           | Tauto
           | Tbreak
           | Tcase
           | Tchar
           | Tconst
           | Tcontinue
           | Tdefault
           | Tdo
           | Tdouble
           | Telse
           | Tenum
           | Textern
           | Tfloat
           | Tfor
           | Tgoto
           | Tif
           | Tint
           | Tlong
           | Tregister
           | Treturn
           | Tshort
           | Tsigned
           | Tsizeof
           | Tstatic
           | Tstruct
           | Tswitch
           | Ttypedef
           | Tunion
           | Tunsigned
           | Tvoid
           | Tvolatile
           | Twhile

           | Ttypename

           | Tanti_id String
           | Tanti_const String
           | Tanti_int String
           | Tanti_uint String
           | Tanti_lint String
           | Tanti_ulint String
           | Tanti_llint String
           | Tanti_ullint String
           | Tanti_float String
           | Tanti_double String
           | Tanti_long_double String
           | Tanti_char String
           | Tanti_string String
           | Tanti_exp String
           | Tanti_func String
           | Tanti_args String
           | Tanti_decl String
           | Tanti_decls String
           | Tanti_sdecl String
           | Tanti_sdecls String
           | Tanti_enum String
           | Tanti_enums String
           | Tanti_esc String
           | Tanti_escstm String
           | Tanti_edecl String
           | Tanti_edecls String
           | Tanti_item String
           | Tanti_items String
           | Tanti_stm String
           | Tanti_stms String
           | Tanti_type_qual String
           | Tanti_type_quals String
           | Tanti_type String
           | Tanti_spec String
           | Tanti_param String
           | Tanti_params String
           | Tanti_pragma String
           | Tanti_comment String
           | Tanti_init String
           | Tanti_inits String
           | Tanti_attr String
           | Tanti_attrs String

           -- C99
           | TBool
           | TComplex
           | TImaginary
           | Tinline
           | Trestrict

           -- GCC
           | Tasm
           | Tattribute
           | Tbuiltin_va_arg
           | Tbuiltin_va_list
           | Textension
           | Ttypeof
           | T__restrict

           -- CUDA
           | TCUDAmutable
           | TCUDA3lt
           | TCUDA3gt
           | TCUDAdevice
           | TCUDAglobal
           | TCUDAhost
           | TCUDAconstant
           | TCUDAshared
           | TCUDArestrict
           | TCUDAnoinline

           -- OpenCL
           | TCLprivate
           | TCLlocal
           | TCLglobal
           | TCLconstant
           | TCLreadonly
           | TCLwriteonly
           | TCLkernel

           -- Clang (currently active is Objective-C is active)
           | T__block

           -- Objective-C
           | TObjCnamed String
           | TObjCat
           | TObjCautoreleasepool
           | TObjCcatch
           | TObjCclass
           | TObjCcompatibility_alias
           | TObjCdynamic
           | TObjCencode
           | TObjCend
           | TObjCfinally
           | TObjCimplementation
           | TObjCinterface
           | TObjCNO
           | TObjCprivate
           | TObjCoptional
           | TObjCpublic
           | TObjCproperty
           | TObjCprotected
           | TObjCprotocol
           | TObjCpackage
           | TObjCrequired
           | TObjCselector
           | TObjCsynchronized
           | TObjCsynthesize
           | TObjCthrow
           | TObjCtry
           | TObjCYES
           | TObjC__weak
           | TObjC__strong
           | TObjC__unsafe_unretained

           | Tanti_objc_ifdecl String
           | Tanti_objc_ifdecls String
           | Tanti_objc_prop String
           | Tanti_objc_props String
           | Tanti_objc_prop_attr String
           | Tanti_objc_prop_attrs String
           | Tanti_objc_dicts String
           | Tanti_objc_param String
           | Tanti_objc_params String
           | Tanti_objc_method_proto String
           | Tanti_objc_method_def String
           | Tanti_objc_method_defs String
           | Tanti_objc_recv String
           | Tanti_objc_arg String
           | Tanti_objc_args String
    deriving (Ord, Eq)

instance Pretty Token where
    ppr = text . show

instance Show Token where
    show Teof                           = "EOF"
    show (Tpragma s)                    = "#pragma " ++ s
    show (Tcomment s)                   = s
    show (TintConst (s, _, _))          = s
    show (TlongIntConst (s, _, _))      = s
    show (TlongLongIntConst (s, _, _))  = s
    show (TfloatConst (s, _))           = s
    show (TdoubleConst (s, _))          = s
    show (TlongDoubleConst (s, _))      = s
    show (TcharConst (s, _))            = s
    show (TstringConst (s, _))          = s
    show (Tidentifier s)                = s
    show (Tnamed s)                     = s

    show (Tanti_id s)                   = showAnti "id"  s
    show (Tanti_const s)                = showAnti "const"  s
    show (Tanti_int s)                  = showAnti "int"  s
    show (Tanti_uint s)                 = showAnti "uint"  s
    show (Tanti_lint s)                 = showAnti "lint"  s
    show (Tanti_ulint s)                = showAnti "ulint"  s
    show (Tanti_llint s)                = showAnti "llint"  s
    show (Tanti_ullint s)               = showAnti "ullint"  s
    show (Tanti_float s)                = showAnti "float"  s
    show (Tanti_double s)               = showAnti "double"  s
    show (Tanti_long_double s)          = showAnti "longdouble"  s
    show (Tanti_char s)                 = showAnti "char"  s
    show (Tanti_string s)               = showAnti "string"  s
    show (Tanti_exp s)                  = showAnti "exp"  s
    show (Tanti_func s)                 = showAnti "func"  s
    show (Tanti_args s)                 = showAnti "args"  s
    show (Tanti_decl s)                 = showAnti "decl"  s
    show (Tanti_decls s)                = showAnti "decls"  s
    show (Tanti_sdecl s)                = showAnti "sdecl"  s
    show (Tanti_sdecls s)               = showAnti "sdecls"  s
    show (Tanti_enum s)                 = showAnti "enum"  s
    show (Tanti_enums s)                = showAnti "enums"  s
    show (Tanti_esc s)                  = showAnti "esc"  s
    show (Tanti_escstm s)               = showAnti "escstm"  s
    show (Tanti_edecl s)                = showAnti "edecl"  s
    show (Tanti_edecls s)               = showAnti "edecls"  s
    show (Tanti_item s)                 = showAnti "item"  s
    show (Tanti_items s)                = showAnti "items"  s
    show (Tanti_stm s)                  = showAnti "stm"  s
    show (Tanti_stms s)                 = showAnti "stms"  s
    show (Tanti_type_quals s)           = showAnti "tyquals" s
    show (Tanti_type_qual s)            = showAnti "tyqual" s
    show (Tanti_type s)                 = showAnti "ty"  s
    show (Tanti_spec s)                 = showAnti "spec"  s
    show (Tanti_param s)                = showAnti "param"  s
    show (Tanti_params s)               = showAnti "params"  s
    show (Tanti_pragma s)               = showAnti "pragma"  s
    show (Tanti_comment s)              = showAnti "comment"  s
    show (Tanti_init s)                 = showAnti "init"  s
    show (Tanti_inits s)                = showAnti "inits"  s
    show (Tanti_attr s)                 = showAnti "attr"  s
    show (Tanti_attrs s)                = showAnti "attrs"  s

    --
    -- Objective C
    --
    show (TObjCnamed s)              = s

    show (Tanti_objc_ifdecl s)       = showAnti "ifdecl" s
    show (Tanti_objc_ifdecls s)      = showAnti "ifdecls" s
    show (Tanti_objc_prop s)         = showAnti "prop" s
    show (Tanti_objc_props s)        = showAnti "props" s
    show (Tanti_objc_prop_attr s)    = showAnti "propattr" s
    show (Tanti_objc_prop_attrs s)   = showAnti "propattrs" s
    show (Tanti_objc_dicts s)        = showAnti "dictelems" s
    show (Tanti_objc_param s)        = showAnti "methparam" s
    show (Tanti_objc_params s)       = showAnti "methparams" s
    show (Tanti_objc_method_proto s) = showAnti "methproto" s
    show (Tanti_objc_method_def s)   = showAnti "methdef" s
    show (Tanti_objc_method_defs s)  = showAnti "methdefs" s
    show (Tanti_objc_recv s)         = showAnti "recv" s
    show (Tanti_objc_arg s)          = showAnti "kwarg" s
    show (Tanti_objc_args s)         = showAnti "kwargs" s

    show t = fromMaybe (error "language-c-quote: internal error: unknown token")
                       (lookup t tokenStrings)

showAnti :: String -> String -> String
showAnti anti s =
    "$" ++ anti ++ ":" ++
    if isIdentifier s then s else "(" ++ s ++ ")"
  where
    isIdentifier :: String -> Bool
    isIdentifier []       = False
    isIdentifier ('_':cs) = all isIdChar cs
    isIdentifier (c:cs)   = isLower c && all isIdChar cs

    isIdChar :: Char -> Bool
    isIdChar '_' = True
    isIdChar c   = isAlphaNum c

tokenStrings :: [(Token, String)]
tokenStrings = [(Tlparen,     "("),
                (Trparen,     ")"),
                (Tlbrack,     "["),
                (Trbrack,     "]"),
                (Tlbrace,     "{"),
                (Trbrace,     "}"),
                (Tcomma,      ","),
                (Tsemi,       ";"),
                (Tcolon,      ":"),
                (Tquestion,   "?"),
                (Tdot,        "."),
                (Tarrow,      "->"),
                (Tellipses,   "..."),
                (Tplus,       "+"),
                (Tminus,      "-"),
                (Tstar,       "*"),
                (Tdiv,        "/"),
                (Tmod,        "%"),
                (Tnot,        "~"),
                (Tand,        "&"),
                (Tor,         "|"),
                (Txor,        "^"),
                (Tlsh,        "<<"),
                (Trsh,        ">>"),
                (Tinc,        "++"),
                (Tdec,        "--"),
                (Tlnot,       "!"),
                (Tland,       "&&"),
                (Tlor,        "||"),
                (Teq,         "=="),
                (Tne,         "!="),
                (Tlt,         "<"),
                (Tgt,         ">"),
                (Tle,         "<="),
                (Tge,         ">="),
                (Tassign,     "="),
                (Tadd_assign, "+="),
                (Tsub_assign, "-="),
                (Tmul_assign, "*="),
                (Tdiv_assign, "/="),
                (Tmod_assign, "%="),
                (Tlsh_assign, "<<="),
                (Trsh_assign, ">>="),
                (Tand_assign, "&="),
                (Tor_assign,  "|="),
                (Txor_assign, "^="),

                --
                -- Keywords
                --
                (Tauto,      "auto"),
                (Tbreak,     "break"),
                (Tcase,      "case"),
                (Tchar,      "char"),
                (Tconst,     "const"),
                (Tcontinue,  "continue"),
                (Tdefault,   "default"),
                (Tdo,        "do"),
                (Tdouble,    "double"),
                (Telse,      "else"),
                (Tenum,      "enum"),
                (Textern,    "extern"),
                (Tfloat,     "float"),
                (Tfor,       "for"),
                (Tgoto,      "goto"),
                (Tif,        "if"),
                (Tint,       "int"),
                (Tlong,      "long"),
                (Tregister,  "register"),
                (Treturn,    "return"),
                (Tshort,     "short"),
                (Tsigned,    "signed"),
                (Tsizeof,    "sizeof"),
                (Tstatic,    "static"),
                (Tstruct,    "struct"),
                (Tswitch,    "switch"),
                (Ttypedef,   "typedef"),
                (Tunion,     "union"),
                (Tunsigned,  "unsigned"),
                (Tvoid,      "void"),
                (Tvolatile,  "volatile"),
                (Twhile,     "while"),

                (Ttypename,  "typename"),

                --
                -- C99 extensions
                --
                (TBool,      "_Bool"),
                (TComplex,   "_TComplex"),
                (TImaginary, "_TImaginary"),
                (Tinline,    "inline"),
                (Trestrict,  "restrict"),

                --
                -- GCC extensions
                --
                (Tasm,             "asm"),
                (Tattribute,       "__attribute__"),
                (Tbuiltin_va_arg,  "__builtin_va_arg"),
                (Tbuiltin_va_list, "__builtin_va_list"),
                (Textension,       "__extension__"),
                (Ttypeof,          "typeof"),
                (T__restrict,      "__restrict"),

                --
                -- Clang extensions
                --
                (T__block , "__block"),

                --
                -- Objective-C extensions
                --
                (TObjCat                   , "@"),
                (TObjCautoreleasepool      , "autoreleasepool"),
                (TObjCcatch                , "catch"),
                (TObjCclass                , "class"),
                (TObjCcompatibility_alias  , "compatibility_alias"),
                (TObjCdynamic              , "dynamic"),
                (TObjCencode               , "encode"),
                (TObjCend                  , "end"),
                (TObjCfinally              , "finally"),
                (TObjCimplementation       , "implementation"),
                (TObjCinterface            , "interface"),
                (TObjCNO                   , "NO"),
                (TObjCoptional             , "optional"),
                (TObjCprivate              , "private"),
                (TObjCpublic               , "public"),
                (TObjCproperty             , "property"),
                (TObjCprotected            , "protected"),
                (TObjCprotocol             , "protocol"),
                (TObjCpackage              , "package"),
                (TObjCrequired             , "required"),
                (TObjCselector             , "selector"),
                (TObjCsynchronized         , "synchronized"),
                (TObjCsynthesize           , "synthesize"),
                (TObjCthrow                , "throw"),
                (TObjCtry                  , "try"),
                (TObjCYES                  , "YES"),
                (TObjC__weak               , "__weak"),
                (TObjC__strong             , "__strong"),
                (TObjC__unsafe_unretained  , "__unsafe_unretained"),

                --
                -- CUDA extensions
                --
                (TCUDAmutable,  "mutable"),
                (TCUDAdevice,   "__device__"),
                (TCUDAglobal,   "__global__"),
                (TCUDAhost,     "__host__"),
                (TCUDAconstant, "__constant__"),
                (TCUDAshared,   "__shared__"),
                (TCUDArestrict, "__restrict__"),
                (TCUDAnoinline, "__noinline__"),

                --
                -- OpenCL extensions
                --
                (TCLprivate,   "private"),    -- must be without '__' prefix for Objective-C
                (TCLlocal,     "__local"),
                (TCLglobal,    "__global"),
                (TCLconstant,  "__constant"),
                (TCLreadonly,  "read_only"),
                (TCLwriteonly, "write_only"),
                (TCLkernel,    "__kernel")
                ]

keywords :: [(String,      Token,      Maybe [Extensions])]
keywords = [("auto",       Tauto,      Nothing),
            ("break",      Tbreak,     Nothing),
            ("case",       Tcase,      Nothing),
            ("char",       Tchar,      Nothing),
            ("const",      Tconst,     Nothing),
            ("continue",   Tcontinue,  Nothing),
            ("default",    Tdefault,   Nothing),
            ("do",         Tdo,        Nothing),
            ("double",     Tdouble,    Nothing),
            ("else",       Telse,      Nothing),
            ("enum",       Tenum,      Nothing),
            ("extern",     Textern,    Nothing),
            ("float",      Tfloat,     Nothing),
            ("for",        Tfor,       Nothing),
            ("goto",       Tgoto,      Nothing),
            ("if",         Tif,        Nothing),
            ("int",        Tint,       Nothing),
            ("long",       Tlong,      Nothing),
            ("register",   Tregister,  Nothing),
            ("return",     Treturn,    Nothing),
            ("short",      Tshort,     Nothing),
            ("signed",     Tsigned,    Nothing),
            ("sizeof",     Tsizeof,    Nothing),
            ("static",     Tstatic,    Nothing),
            ("struct",     Tstruct,    Nothing),
            ("switch",     Tswitch,    Nothing),
            ("typedef",    Ttypedef,   Nothing),
            ("union",      Tunion,     Nothing),
            ("unsigned",   Tunsigned,  Nothing),
            ("void",       Tvoid,      Nothing),
            ("volatile",   Tvolatile,  Nothing),
            ("while",      Twhile,     Nothing),

            --
            -- C99
            --
            ("_Bool",      TBool,      Nothing),
            ("_Complex",   TComplex,   Nothing),
            ("_Imaginary", TImaginary, Nothing),
            ("inline",     Tinline,    Nothing),
            ("restrict",   Trestrict,  Nothing),

            --
            -- GCC
            --
            ("asm",               Tasm,             Just [Gcc]),
            ("__asm",             Tasm,             Just [Gcc]),
            ("__asm__",           Tasm,             Just [Gcc]),
            ("__attribute__",     Tattribute,       Just [Gcc]),
            ("__builtin_va_arg",  Tbuiltin_va_arg,  Just [Gcc]),
            ("__builtin_va_list", Tbuiltin_va_list, Just [Gcc]),
            ("__const",           Tconst,           Just [Gcc]),
            ("__const__",         Tconst,           Just [Gcc]),
            ("__inline",          Tinline,          Just [Gcc]),
            ("__inline__",        Tinline,          Just [Gcc]),
            ("__restrict",        T__restrict,      Just [Gcc]),
            ("__restrict__",      T__restrict,      Just [Gcc]),
            ("typeof",            Ttypeof,          Just [Gcc]),
            ("__typeof",          Ttypeof,          Just [Gcc]),
            ("__typeof__",        Ttypeof,          Just [Gcc]),
            ("__volatile",        Tvolatile,        Just [Gcc]),
            ("__volatile__",      Tvolatile,        Just [Gcc]),

            --
            -- Clang blocks
            --
            ("__block", T__block, Just [Blocks, ObjC]),

            --
            -- Objective-C
            --
            ("autoreleasepool",     TObjCautoreleasepool,     Just [ObjC]),
            ("catch",               TObjCcatch,               Just [ObjC]),
            ("class",               TObjCclass,               Just [ObjC]),
            ("compatibility_alias", TObjCcompatibility_alias, Just [ObjC]),
            ("dynamic",             TObjCdynamic,             Just [ObjC]),
            ("encode",              TObjCencode,              Just [ObjC]),
            ("end",                 TObjCend,                 Just [ObjC]),
            ("finally",             TObjCfinally,             Just [ObjC]),
            ("implementation",      TObjCimplementation,      Just [ObjC]),
            ("interface",           TObjCinterface,           Just [ObjC]),
            ("NO",                  TObjCNO,                  Just [ObjC]),
            ("optional",            TObjCoptional,            Just [ObjC]),
            ("public",              TObjCpublic,              Just [ObjC]),
            ("property",            TObjCproperty,            Just [ObjC]),
            ("protected",           TObjCprotected,           Just [ObjC]),
            ("package",             TObjCpackage,             Just [ObjC]),
            ("protocol",            TObjCprotocol,            Just [ObjC]),
            ("required",            TObjCrequired,            Just [ObjC]),
            ("selector",            TObjCselector,            Just [ObjC]),
            ("synchronized",        TObjCsynchronized,        Just [ObjC]),
            ("synthesize",          TObjCsynthesize,          Just [ObjC]),
            ("throw",               TObjCthrow,               Just [ObjC]),
            ("try",                 TObjCtry,                 Just [ObjC]),
            ("YES",                 TObjCYES,                 Just [ObjC]),
            ("__weak",              TObjC__weak,              Just [ObjC]),
            ("__strong",            TObjC__strong,            Just [ObjC]),
            ("__unsafe_unretained", TObjC__unsafe_unretained, Just [ObjC]),

            --
            -- CUDA
            --
            ("mutable",      TCUDAmutable,  Just [CUDA]),
            ("__device__",   TCUDAdevice,   Just [CUDA]),
            ("__global__",   TCUDAglobal,   Just [CUDA]),
            ("__host__",     TCUDAhost,     Just [CUDA]),
            ("__constant__", TCUDAconstant, Just [CUDA]),
            ("__shared__",   TCUDAshared,   Just [CUDA]),
            ("__restrict__", TCUDArestrict, Just [CUDA]),
            ("__noinline__", TCUDAnoinline, Just [CUDA]),

            --
            -- OpenCL
            --
            ("private",      TCLprivate,   Just [OpenCL, ObjC]),  -- see Lexer.identifier for 'TObjCprivate'
            ("__private",    TCLprivate,   Just [OpenCL]),
            ("local",        TCLlocal,     Just [OpenCL]),
            ("__local",      TCLlocal,     Just [OpenCL]),
            ("global",       TCLglobal,    Just [OpenCL]),
            ("__global",     TCLglobal,    Just [OpenCL]),
            ("constant",     TCLconstant,  Just [OpenCL]),
            ("__constant",   TCLconstant,  Just [OpenCL]),
            ("read_only",    TCLreadonly,  Just [OpenCL]),
            ("__read_only",  TCLreadonly,  Just [OpenCL]),
            ("write_only",   TCLwriteonly, Just [OpenCL]),
            ("__write_only", TCLwriteonly, Just [OpenCL]),
            ("kernel",       TCLkernel,    Just [OpenCL]),
            ("__kernel",     TCLkernel,    Just [OpenCL])
           ]

type ExtensionsInt = Word32

keywordMap :: Map.Map String (Token, Maybe ExtensionsInt)
keywordMap = Map.fromList (map f keywords)
  where
    f  ::  (String, Token, Maybe [Extensions])
       ->  (String, (Token, Maybe ExtensionsInt))
    f (s, t, Nothing)    = (s, (t, Nothing))
    f (s, t, Just exts)  = (s, (t, Just i))
      where
        i = foldl' setBit 0 (map fromEnum exts)
