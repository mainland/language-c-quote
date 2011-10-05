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
-- Module      :  Language.C.Parser.Tokens
-- Copyright   :  (c) Harvard University 2006-2010
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

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

import Language.C.Syntax (Extensions(..),
                          Signed(..))

data Token = Teof
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
           | T3lt
           | T3gt
           | Tdevice
           | Tglobal
           | Thost
           | Tconstant
           | Tshared
           | Tnoinline

           -- Antiquoting
           | Ttypename

           | Tanti_id String
           | Tanti_int String
           | Tanti_uint String
           | Tanti_lint String
           | Tanti_ulint String
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
    deriving (Ord, Eq)

instance Show Token where
    show Teof                           = "EOF"
    show (TintConst (s, _, _))          = s
    show (TlongIntConst (s, _, _))      = s
    show (TlongLongIntConst (s, _, _))  = s
    show (TfloatConst (s, _))           = s
    show (TdoubleConst (s, _))          = s
    show (TlongDoubleConst (s, _))      = s
    show (TcharConst (s, _))            = s
    show (TstringConst (s, _))          = s
    show (Tidentifier s)                = "identifier: " ++ show s
    show t = fromMaybe (error "internal error: unknown token")
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
                (Tdevice,   "__device__"),
                (Tglobal,   "__global__"),
                (Thost,     "__host__"),
                (Tconstant, "__constant__"),
                (Tshared,   "__shared__"),
                (Tnoinline, "__noinline__")
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

            ("__device__",   Tdevice,   Just [CUDA]),
            ("__global__",   Tglobal,   Just [CUDA]),
            ("__host__",     Thost,     Just [CUDA]),
            ("__constant__", Tconstant, Just [CUDA]),
            ("__shared__",   Tshared,   Just [CUDA]),
            ("__noinline__", Tnoinline, Just [CUDA])
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
