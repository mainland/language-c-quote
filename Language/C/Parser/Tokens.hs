-- |
-- Module      :  Language.C.Parser.Tokens
-- Copyright   :  (c) Harvard University 2006-2011
--                (c) Geoffrey Mainland 2011-2013
--                (c) Manuel M T Chakravarty 2013
--                (c) Drexel University 2013
-- License     :  BSD-style
-- Maintainer  :  mainland@cs.drexel.edu

module Language.C.Parser.Tokens (
    Token(..),
    ExtensionsInt,
    keywords,
    keywordMap
  ) where

import Data.Bits
import Data.List (foldl')
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Word
import Text.PrettyPrint.Mainland

import Language.C.Syntax (Extensions(..),
                          Signed(..))

data Token = Teof
           | Tpragma String
           | TintConst (String, Signed, Integer)
           | TlongIntConst (String, Signed, Integer)
           | TlongLongIntConst (String, Signed, Integer)
           | TfloatConst (String, Rational)
           | TdoubleConst (String, Rational)
           | TlongDoubleConst (String, Rational)
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
           | Tinline
           | Tint
           | Tlong
           | Tregister
           | Trestrict
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
           | TBool
           | TComplex
           | TImaginary

           -- GCC
           | Tasm
           | Tattribute
           | Tbuiltin_va_arg
           | Tbuiltin_va_list
           | Textension
           | Ttypeof

           -- CUDA
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
           | TObjC__unsafe_retained

           -- Antiquoting
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
           | Tanti_edecl String
           | Tanti_edecls String
           | Tanti_item String
           | Tanti_items String
           | Tanti_stm String
           | Tanti_stms String
           | Tanti_type String
           | Tanti_spec String
           | Tanti_param String
           | Tanti_params String
           | Tanti_pragma String
           | Tanti_init String
           | Tanti_inits String
    deriving (Ord, Eq)

instance Pretty Token where
    ppr = text . show

instance Show Token where
    show Teof                           = "EOF"
    show (Tpragma s)                    = "#pragma " ++ s
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
    show (TObjCnamed s)                 = s

    show (Tanti_id s)                   = "$id:" ++ s
    show (Tanti_const s)                = "$const:" ++ s
    show (Tanti_int s)                  = "$int:" ++ s
    show (Tanti_uint s)                 = "$uint:" ++ s
    show (Tanti_lint s)                 = "$lint:" ++ s
    show (Tanti_ulint s)                = "$ulint:" ++ s
    show (Tanti_llint s)                = "$llint:" ++ s
    show (Tanti_ullint s)               = "$ullint:" ++ s
    show (Tanti_float s)                = "$float:" ++ s
    show (Tanti_double s)               = "$double:" ++ s
    show (Tanti_long_double s)          = "$longdouble:" ++ s
    show (Tanti_char s)                 = "$char:" ++ s
    show (Tanti_string s)               = "$string:" ++ s
    show (Tanti_exp s)                  = "$exp:" ++ s
    show (Tanti_func s)                 = "$func:" ++ s
    show (Tanti_args s)                 = "$args:" ++ s
    show (Tanti_decl s)                 = "$decl:" ++ s
    show (Tanti_decls s)                = "$decls:" ++ s
    show (Tanti_sdecl s)                = "$sdecl:" ++ s
    show (Tanti_sdecls s)               = "$sdecls:" ++ s
    show (Tanti_enum s)                 = "$enum:" ++ s
    show (Tanti_enums s)                = "$enums:" ++ s
    show (Tanti_esc s)                  = "$esc:" ++ s
    show (Tanti_edecl s)                = "$edecl:" ++ s
    show (Tanti_edecls s)               = "$edecls:" ++ s
    show (Tanti_item s)                 = "$item:" ++ s
    show (Tanti_items s)                = "$items:" ++ s
    show (Tanti_stm s)                  = "$stm:" ++ s
    show (Tanti_stms s)                 = "$stms:" ++ s
    show (Tanti_type s)                 = "$ty:" ++ s
    show (Tanti_spec s)                 = "$spec:" ++ s
    show (Tanti_param s)                = "$param:" ++ s
    show (Tanti_params s)               = "$params:" ++ s
    show (Tanti_pragma s)               = "$pragma:" ++ s
    show (Tanti_init s)                 = "$init:" ++ s
    show (Tanti_inits s)                = "$inits:" ++ s
    show t = fromMaybe (error "language-c-quote: internal error: unknown token")
                       (lookup t tokenStrings)

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
                (Tinline,    "inline"),
                (Tint,       "int"),
                (Tlong,      "long"),
                (Tregister,  "register"),
                (Trestrict,  "restrict"),
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
                (TBool,      "_Bool"),
                (TComplex,   "_TComplex"),
                (TImaginary, "_TImaginary"),

                --
                -- GCC extensions
                --
                (Tasm,             "asm"),
                (Tattribute,       "__attribute__"),
                (Tbuiltin_va_arg,  "__builtin_va_arg"),
                (Tbuiltin_va_list, "__builtin_va_list"),
                (Textension,       "__extension__"),
                (Ttypeof,          "typeof"),

                --
                -- CUDA extensions
                --
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
                (TCLkernel,    "__kernel"),

                --
                -- Clang extensions
                --
                (T__block                , "__block"),

                --
                -- Objective-C extensions
                --
                (TObjCat                 , "@"),
                (TObjCautoreleasepool    , "autoreleasepool"),
                (TObjCcatch              , "catch"),
                (TObjCclass              , "class"),
                (TObjCcompatibility_alias, "compatibility_alias"),
                (TObjCdynamic            , "dynamic"),
                (TObjCencode             , "encode"),
                (TObjCend                , "end"),
                (TObjCfinally            , "finally"),
                (TObjCimplementation     , "implementation"),
                (TObjCinterface          , "interface"),
                (TObjCNO                 , "NO"),
                (TObjCoptional           , "optional"),
                (TObjCprivate            , "private"),
                (TObjCpublic             , "public"),
                (TObjCproperty           , "property"),
                (TObjCprotected          , "protected"),
                (TObjCprotocol           , "protocol"),
                (TObjCpackage            , "package"),
                (TObjCrequired           , "required"),
                (TObjCselector           , "selector"),
                (TObjCsynchronized       , "synchronized"),
                (TObjCsynthesize         , "synthesize"),
                (TObjCthrow              , "throw"),
                (TObjCtry                , "try"),
                (TObjCYES                , "YES"),
                (TObjC__weak             , "__weak"),
                (TObjC__strong           , "__strong"),
                (TObjC__unsafe_retained  , "__unsafe_retained"),

                (Ttypename, "typename")
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
            ("inline",     Tinline,    Nothing),
            ("int",        Tint,       Nothing),
            ("long",       Tlong,      Nothing),
            ("register",   Tregister,  Nothing),
            ("restrict",   Trestrict,  Nothing),
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
            ("_Bool",      TBool,      Nothing),
            ("_Complex",   TComplex,   Nothing),
            ("_Imaginary", TImaginary, Nothing),

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
            ("__restrict",        Trestrict,        Just [Gcc]),
            ("__restrict__",      Trestrict,        Just [Gcc]),
            ("typeof",            Ttypeof,          Just [Gcc]),
            ("__typeof",          Ttypeof,          Just [Gcc]),
            ("__typeof__",        Ttypeof,          Just [Gcc]),
            ("__volatile",        Tvolatile,        Just [Gcc]),
            ("__volatile__",      Tvolatile,        Just [Gcc]),

            ("__device__",   TCUDAdevice,   Just [CUDA]),
            ("__global__",   TCUDAglobal,   Just [CUDA]),
            ("__host__",     TCUDAhost,     Just [CUDA]),
            ("__constant__", TCUDAconstant, Just [CUDA]),
            ("__shared__",   TCUDAshared,   Just [CUDA]),
            ("__restrict__", TCUDArestrict, Just [CUDA]),
            ("__noinline__", TCUDAnoinline, Just [CUDA]),

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
            ("__kernel",     TCLkernel,    Just [OpenCL]),

            ("__block",             T__block,                 Just [ObjC]),

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
            ("__unsafe_retained",   TObjC__unsafe_retained,   Just [ObjC])
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
