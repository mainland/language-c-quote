-- -*- mode: literate-haskell -*-

{
{-# OPTIONS -w #-}

-- |
-- Module      :  Language.C.Parser.Parser
-- Copyright   :  (c) 2006-2011 Harvard University
--                (c) 2011-2012 Geoffrey Mainland
--                (c) 2013-2014 Manuel M T Chakravarty
--                (c) 2013-2016 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Language.C.Parser.Parser where

import Control.Monad (forM_,
                      when,
                      unless,
                      liftM)
import Control.Monad.Exception
import Data.List (intersperse, sort)
import Data.Loc
import Data.Maybe (fromMaybe, catMaybes)
#if !(MIN_VERSION_base(4,9,0))
import Data.Monoid (Monoid(..), (<>))
#endif /* !(MIN_VERSION_base(4,9,0)) */
#if MIN_VERSION_base(4,9,0) && !(MIN_VERSION_base(4,11,0))
import Data.Semigroup (Semigroup(..))
#endif
import Text.PrettyPrint.Mainland
import Text.PrettyPrint.Mainland.Class

import Language.C.Parser.Lexer
import Language.C.Parser.Monad
import qualified Language.C.Parser.Tokens as T
import Language.C.Pretty
import Language.C.Syntax
import qualified Language.C.Syntax as C
}

%token
 CHAR        { L _ (T.TcharConst _) }
 STRING      { L _ (T.TstringConst _) }
 INT         { L _ (T.TintConst _) }
 LONG        { L _ (T.TlongIntConst _) }
 LONG_LONG   { L _ (T.TlongLongIntConst _) }
 FLOAT       { L _ (T.TfloatConst _) }
 DOUBLE      { L _ (T.TdoubleConst _) }
 LONG_DOUBLE { L _ (T.TlongDoubleConst _) }
 ID          { L _ (T.Tidentifier _) }
 NAMED       { L _ (T.Tnamed _) }

 '('    { L _ T.Tlparen }
 ')'    { L _ T.Trparen }
 '['    { L _ T.Tlbrack }
 ']'    { L _ T.Trbrack }
 '{'    { L _ T.Tlbrace }
 '}'    { L _ T.Trbrace }
 ','    { L _ T.Tcomma }
 ';'    { L _ T.Tsemi }
 ':'    { L _ T.Tcolon }
 '?'    { L _ T.Tquestion }
 '.'    { L _ T.Tdot }
 '->'   { L _ T.Tarrow }
 '...'  { L _ T.Tellipses }

 '+'   { L _ T.Tplus }
 '-'   { L _ T.Tminus }
 '*'   { L _ T.Tstar }
 '/'   { L _ T.Tdiv }
 '%'   { L _ T.Tmod }
 '~'   { L _ T.Tnot }
 '&'   { L _ T.Tand }
 '|'   { L _ T.Tor }
 '^'   { L _ T.Txor }
 '<<'  { L _ T.Tlsh }
 '>>'  { L _ T.Trsh }
 '++'  { L _ T.Tinc }
 '--'  { L _ T.Tdec }

 '!'   { L _ T.Tlnot }
 '&&'  { L _ T.Tland }
 '||'  { L _ T.Tlor }

 '=='  { L _ T.Teq }
 '!='  { L _ T.Tne }
 '<'   { L _ T.Tlt }
 '>'   { L _ T.Tgt }
 '<='  { L _ T.Tle }
 '>='  { L _ T.Tge }

 '='   { L _ T.Tassign }
 '+='  { L _ T.Tadd_assign }
 '-='  { L _ T.Tsub_assign }
 '*='  { L _ T.Tmul_assign }
 '/='  { L _ T.Tdiv_assign }
 '%='  { L _ T.Tmod_assign }
 '<<=' { L _ T.Tlsh_assign }
 '>>=' { L _ T.Trsh_assign }
 '&='  { L _ T.Tand_assign }
 '|='  { L _ T.Tor_assign }
 '^='  { L _ T.Txor_assign }

 'auto'       { L _ T.Tauto }
 'break'      { L _ T.Tbreak }
 'case'       { L _ T.Tcase }
 'char'       { L _ T.Tchar }
 'const'      { L _ T.Tconst }
 'continue'   { L _ T.Tcontinue }
 'default'    { L _ T.Tdefault }
 'do'         { L _ T.Tdo }
 'double'     { L _ T.Tdouble }
 'else'       { L _ T.Telse }
 'enum'       { L _ T.Tenum }
 'extern'     { L _ T.Textern }
 'float'      { L _ T.Tfloat }
 'for'        { L _ T.Tfor }
 'goto'       { L _ T.Tgoto }
 'if'         { L _ T.Tif }
 'int'        { L _ T.Tint }
 'long'       { L _ T.Tlong }
 'register'   { L _ T.Tregister }
 'return'     { L _ T.Treturn }
 'short'      { L _ T.Tshort }
 'signed'     { L _ T.Tsigned }
 'sizeof'     { L _ T.Tsizeof }
 'static'     { L _ T.Tstatic }
 'struct'     { L _ T.Tstruct }
 'switch'     { L _ T.Tswitch }
 'typedef'    { L _ T.Ttypedef }
 'union'      { L _ T.Tunion }
 'unsigned'   { L _ T.Tunsigned }
 'void'       { L _ T.Tvoid }
 'volatile'   { L _ T.Tvolatile }
 'while'      { L _ T.Twhile }

 '#pragma'    { L _ (T.Tpragma _) }

 '//'         { L _ (T.Tcomment _) }

 -- Used to indicate typedef's the parser hasn't seen first-hand
 'typename'   { L _ T.Ttypename }

 ANTI_ID          { L _ (T.Tanti_id _) }
 ANTI_CONST       { L _ (T.Tanti_const _) }
 ANTI_INT         { L _ (T.Tanti_int _) }
 ANTI_UINT        { L _ (T.Tanti_uint _) }
 ANTI_LINT        { L _ (T.Tanti_lint _) }
 ANTI_ULINT       { L _ (T.Tanti_ulint _) }
 ANTI_LLINT       { L _ (T.Tanti_llint _) }
 ANTI_ULLINT      { L _ (T.Tanti_ullint _) }
 ANTI_FLOAT       { L _ (T.Tanti_float _) }
 ANTI_DOUBLE      { L _ (T.Tanti_double _) }
 ANTI_LONG_DOUBLE { L _ (T.Tanti_long_double _) }
 ANTI_CHAR        { L _ (T.Tanti_char _) }
 ANTI_STRING      { L _ (T.Tanti_string _) }
 ANTI_EXP         { L _ (T.Tanti_exp _) }
 ANTI_FUNC        { L _ (T.Tanti_func _) }
 ANTI_ARGS        { L _ (T.Tanti_args _) }
 ANTI_DECL        { L _ (T.Tanti_decl _) }
 ANTI_DECLS       { L _ (T.Tanti_decls _) }
 ANTI_SDECL       { L _ (T.Tanti_sdecl _) }
 ANTI_SDECLS      { L _ (T.Tanti_sdecls _) }
 ANTI_ENUM        { L _ (T.Tanti_enum _) }
 ANTI_ENUMS       { L _ (T.Tanti_enums _) }
 ANTI_ESC         { L _ (T.Tanti_esc _) }
 ANTI_ESCSTM      { L _ (T.Tanti_escstm _) }
 ANTI_EDECL       { L _ (T.Tanti_edecl _) }
 ANTI_EDECLS      { L _ (T.Tanti_edecls _) }
 ANTI_ITEM        { L _ (T.Tanti_item _) }
 ANTI_ITEMS       { L _ (T.Tanti_items _) }
 ANTI_STM         { L _ (T.Tanti_stm _) }
 ANTI_STMS        { L _ (T.Tanti_stms _) }
 ANTI_SPEC        { L _ (T.Tanti_spec _) }
 ANTI_TYPE_QUAL   { L _ (T.Tanti_type_qual _) }
 ANTI_TYPE_QUALS  { L _ (T.Tanti_type_quals _) }
 ANTI_TYPE        { L _ (T.Tanti_type _) }
 ANTI_PARAM       { L _ (T.Tanti_param _) }
 ANTI_PARAMS      { L _ (T.Tanti_params _) }
 ANTI_PRAGMA      { L _ (T.Tanti_pragma _) }
 ANTI_COMMENT     { L _ (T.Tanti_comment _) }
 ANTI_INIT        { L _ (T.Tanti_init _) }
 ANTI_INITS       { L _ (T.Tanti_inits _) }

 --
 -- C99
 --
 'inline'     { L _ T.Tinline }
 'restrict'   { L _ T.Trestrict }
 '_Bool'      { L _ T.TBool }
 '_Complex'   { L _ T.TComplex }
 '_Imaginary' { L _ T.TImaginary }

 --
 -- GCC
 --
 '__asm__'           { L _ T.Tasm }
 '__attribute__'     { L _ T.Tattribute }
 '__extension__'     { L _ T.Textension }
 '__builtin_va_arg'  { L _ T.Tbuiltin_va_arg }
 '__builtin_va_list' { L _ T.Tbuiltin_va_list }
 '__typeof__'        { L _ T.Ttypeof }
 '__restrict'        { L _ T.T__restrict }

 ANTI_ATTR        { L _ (T.Tanti_attr _) }
 ANTI_ATTRS       { L _ (T.Tanti_attrs _) }

 --
 -- Clang blocks
 --
 '__block'           { L _ T.T__block }

 --
 -- Objective-C
 --
 OBJCNAMED             { L _ (T.TObjCnamed _) }
 '@'                   { L _ T.TObjCat }
 'autoreleasepool'     { L _ T.TObjCautoreleasepool }
 'catch'               { L _ T.TObjCcatch }
 'class'               { L _ T.TObjCclass }
 'compatibility_alias' { L _ T.TObjCcompatibility_alias }
 'dynamic'             { L _ T.TObjCdynamic }
 'encode'              { L _ T.TObjCencode }
 'end'                 { L _ T.TObjCend }
 'finally'             { L _ T.TObjCfinally }
 'interface'           { L _ T.TObjCinterface }
 'implementation'      { L _ T.TObjCimplementation }
 'NO'                  { L _ T.TObjCNO }
 'objc_private'        { L _ T.TObjCprivate }
 'optional'            { L _ T.TObjCoptional }
 'public'              { L _ T.TObjCpublic }
 'property'            { L _ T.TObjCproperty }
 'protected'           { L _ T.TObjCprotected }
 'package'             { L _ T.TObjCpackage }
 'protocol'            { L _ T.TObjCprotocol }
 'required'            { L _ T.TObjCrequired }
 'selector'            { L _ T.TObjCselector }
 'synchronized'        { L _ T.TObjCsynchronized }
 'synthesize'          { L _ T.TObjCsynthesize }
 'throw'               { L _ T.TObjCthrow }
 'try'                 { L _ T.TObjCtry }
 'YES'                 { L _ T.TObjCYES }
 '__weak'              { L _ T.TObjC__weak }
 '__strong'            { L _ T.TObjC__strong }
 '__unsafe_unretained' { L _ T.TObjC__unsafe_unretained }

 ANTI_OBJC_IFDECL       { L _ (T.Tanti_objc_ifdecl _) }
 ANTI_OBJC_IFDECLS      { L _ (T.Tanti_objc_ifdecls _) }
 ANTI_OBJC_PROP         { L _ (T.Tanti_objc_prop _) }
 ANTI_OBJC_PROPS        { L _ (T.Tanti_objc_props _) }
 ANTI_OBJC_PROP_ATTR    { L _ (T.Tanti_objc_prop_attr _) }
 ANTI_OBJC_PROP_ATTRS   { L _ (T.Tanti_objc_prop_attrs _) }
 ANTI_OBJC_DICTS        { L _ (T.Tanti_objc_dicts _) }
 ANTI_OBJC_PARAM        { L _ (T.Tanti_objc_param _) }
 ANTI_OBJC_PARAMS       { L _ (T.Tanti_objc_params _) }
 ANTI_OBJC_METHOD_PROTO { L _ (T.Tanti_objc_method_proto _) }
 ANTI_OBJC_METHOD_DEF   { L _ (T.Tanti_objc_method_def _) }
 ANTI_OBJC_METHOD_DEFS  { L _ (T.Tanti_objc_method_defs _) }
 ANTI_OBJC_RECV         { L _ (T.Tanti_objc_recv _) }
 ANTI_OBJC_ARG          { L _ (T.Tanti_objc_arg _) }
 ANTI_OBJC_ARGS         { L _ (T.Tanti_objc_args _) }

 --
 -- CUDA
 --
 'mutable'       { L _ T.TCUDAmutable }
 '<<<'           { L _ T.TCUDA3lt }
 '>>>'           { L _ T.TCUDA3gt }
 '__device__'    { L _ T.TCUDAdevice }
 '__global__'    { L _ T.TCUDAglobal }
 '__host__'      { L _ T.TCUDAhost }
 '__constant__'  { L _ T.TCUDAconstant }
 '__shared__'    { L _ T.TCUDAshared }
 '__restrict__'  { L _ T.TCUDArestrict }
 '__noinline__'  { L _ T.TCUDAnoinline }

 --
 -- OpenCL
 --
 'private'      { L _ T.TCLprivate }
 '__private'    { L _ T.TCLprivate }
 'local'        { L _ T.TCLlocal }
 '__local'      { L _ T.TCLlocal }
 'global'       { L _ T.TCLglobal }
 '__global'     { L _ T.TCLglobal }
 'constant'     { L _ T.TCLconstant }
 '__constant'   { L _ T.TCLconstant }
 'read_only'    { L _ T.TCLreadonly }
 '__read_only'  { L _ T.TCLreadonly }
 'write_only'   { L _ T.TCLwriteonly }
 '__write_only' { L _ T.TCLwriteonly }
 'kernel'       { L _ T.TCLkernel }
 '__kernel'     { L _ T.TCLkernel }

-- Three shift-reduce conflicts:
-- (1) Documented conflict in 'objc_protocol_declaration'
-- (2) Objective-C exception syntax (would need lookahead of 2 to disambiguate properly)
-- (3) The standard dangling else conflict
%expect 3

%monad { P } { >>= } { return }
%lexer { lexer } { L _ T.Teof }
%tokentype { (L T.Token) }
%error { happyError }

%name parseExp        expression

%name parseEdecl      external_declaration
%name parseDecl       declaration
%name parseStructDecl struct_declaration
%name parseEnum       enumerator

%name parseTypeQuals type_qualifier_list
%name parseType      type_declaration
%name parseParam     parameter_declaration
%name parseParams    parameter_list
%name parseInit      initializer

%name parseStm        statement
%name parseStms       statement_list
%name parseBlockItem  block_item
%name parseBlockItems block_item_list

%name parseUnit       translation_unit
%name parseFunc       function_definition

%name parseAttr       attrib

--
-- Objective-C
--
%name parseObjCProp        objc_property_decl
%name parseObjCIfaceDecls  objc_interface_decl_list
%name parseObjCImplDecls   objc_implementation_decl_list
%name parseObjCDictElem    objc_key_value
%name parseObjCPropAttr    objc_property_attr
%name parseObjCMethodParam objc_method_param
%name parseObjCMethodProto objc_method_proto
%name parseObjCMethodDef   objc_method_definition
%name parseObjCMethodRecv  objc_receiver
%name parseObjCKeywordArg  objc_keywordarg

%right NAMED OBJCNAMED

%%

{------------------------------------------------------------------------------
 -
 - Notes on the grammar
 -

 - A rule with a '_nla' suffix is a variant of the rule without the suffix such
 - that it does not contain a leading '__attribute__' specifier. We need these
 - rules to prevent an ambiguity in old-style function declarations. See the
 - rule 'declaration_list'.
 -
 - A rule with a '_nlt' suffix is a variant of the rule without the suffix such
 - that it does not contain a leading typedef name. We need these rules to
 - prevent ambiguity in expressions that re-use a typedef name as an identifier.
 -
 ------------------------------------------------------------------------------}

{------------------------------------------------------------------------------
 -
 - Identifiers
 -
 ------------------------------------------------------------------------------}

identifier :: { Id }
identifier :
    ID                    { Id (getID $1) (srclocOf $1) }
  | ANTI_ID               { AntiId (getANTI_ID $1) (srclocOf $1) }

  -- Objective-C
  | 'autoreleasepool'     { Id "autoreleasepool" (srclocOf $1) }
  | 'catch'               { Id "catch" (srclocOf $1) }
  | 'class'               { Id "class" (srclocOf $1) }
  | 'compatibility_alias' { Id "compatibility_alias" (srclocOf $1) }
  | 'dynamic'             { Id "dynamic" (srclocOf $1) }
  | 'encode'              { Id "encode" (srclocOf $1) }
  | 'end'                 { Id "end" (srclocOf $1) }
  | 'finally'             { Id "finally" (srclocOf $1) }
  | 'implementation'      { Id "implementation" (srclocOf $1) }
  | 'interface'           { Id "interface" (srclocOf $1) }
  | 'NO'                  { Id "NO" (srclocOf $1) }
  | 'objc_private'        { Id "private" (srclocOf $1) }
  | 'optional'            { Id "optional" (srclocOf $1) }
  | 'public'              { Id "public" (srclocOf $1) }
  | 'property'            { Id "property" (srclocOf $1) }
  | 'protected'           { Id "protected" (srclocOf $1) }
  | 'package'             { Id "package" (srclocOf $1) }
  | 'protocol'            { Id "protocol" (srclocOf $1) }
  | 'required'            { Id "required" (srclocOf $1) }
  | 'selector'            { Id "selector" (srclocOf $1) }
  | 'synchronized'        { Id "synchronized" (srclocOf $1) }
  | 'synthesize'          { Id "synthesize" (srclocOf $1) }
  | 'throw'               { Id "throw" (srclocOf $1) }
  | 'try'                 { Id "try" (srclocOf $1) }
  | 'YES'                 { Id "YES" (srclocOf $1) }

identifier_or_typedef :: { Id }
identifier_or_typedef :
    identifier  { $1 }
  | NAMED       { Id (getNAMED $1) (srclocOf $1) }

  -- Objective-C
  | OBJCNAMED   { Id (getOBJCNAMED $1) (srclocOf $1) }

{------------------------------------------------------------------------------
 -
 - Constants
 -
 ------------------------------------------------------------------------------}

constant :: { Const }
constant :
    INT               { let (s, sign, n) = getINT $1
                        in
                          IntConst s sign n (srclocOf $1)
                      }
  | LONG              { let (s, sign, n) = getLONG $1
                        in
                          LongIntConst s sign n (srclocOf $1)
                      }
  | LONG_LONG         { let (s, sign, n) = getLONG_LONG $1
                        in
                          LongLongIntConst s sign n (srclocOf $1)
                      }
  | FLOAT             { let (s, n) = getFLOAT $1
                        in
                          FloatConst s n (srclocOf $1)
                      }
  | DOUBLE            { let (s, n) = getDOUBLE $1
                        in
                          DoubleConst s n (srclocOf $1)
                      }
  | LONG_DOUBLE       { let (s, n) = getLONG_DOUBLE $1
                        in
                          LongDoubleConst s n (srclocOf $1)
                      }
  | CHAR              { let (s, c) = getCHAR $1
                        in
                          CharConst s c (srclocOf $1)
                      }
  | ANTI_CONST        { AntiConst      (getANTI_CONST $1)       (srclocOf $1) }
  | ANTI_INT          { AntiInt        (getANTI_INT $1)         (srclocOf $1) }
  | ANTI_UINT         { AntiUInt       (getANTI_UINT $1)        (srclocOf $1) }
  | ANTI_LINT         { AntiLInt       (getANTI_LINT $1)        (srclocOf $1) }
  | ANTI_ULINT        { AntiULInt      (getANTI_ULINT $1)       (srclocOf $1) }
  | ANTI_LLINT        { AntiLLInt      (getANTI_LLINT $1)       (srclocOf $1) }
  | ANTI_ULLINT       { AntiULLInt     (getANTI_ULLINT $1)      (srclocOf $1) }
  | ANTI_FLOAT        { AntiFloat      (getANTI_FLOAT $1)       (srclocOf $1) }
  | ANTI_DOUBLE       { AntiDouble     (getANTI_DOUBLE $1)      (srclocOf $1) }
  | ANTI_LONG_DOUBLE  { AntiLongDouble (getANTI_LONG_DOUBLE $1) (srclocOf $1) }
  | ANTI_CHAR         { AntiChar       (getANTI_CHAR $1)        (srclocOf $1) }
  | ANTI_STRING       { AntiString     (getANTI_STRING $1)      (srclocOf $1) }

{------------------------------------------------------------------------------
 -
 - Expressions
 -
 ------------------------------------------------------------------------------}

lbrace :: { L T.Token }
lbrace :
    '{'         { L (locOf $1) T.Tlbrace }
  | '{' comment { L (locOf $1) T.Tlbrace }

semi :: { L T.Token }
semi :
    ';'         { L (locOf $1) T.Tsemi }
  | ';' comment { L (locOf $1) T.Tsemi }

string_literal :: { StringLit }
string_literal :
    string_literal_rlist
      { let { slits = rev $1
            ; raw   = map (fst . unLoc) slits
            ; s     = (concat . map (snd . unLoc)) slits
            }
        in
         StringLit raw s (srclocOf slits)
      }

string_literal_rlist :: { RevList (L (String, String)) }
string_literal_rlist :
    STRING                      { rsingleton (L (locOf $1) (getSTRING $1)) }
  | string_literal_rlist STRING { rcons (L (locOf $2) (getSTRING $2)) $1 }

primary_expression :: { Exp }
primary_expression :
    identifier_or_typedef
      { Var $1 (srclocOf $1) }
  | constant
      { Const $1 (srclocOf $1) }
  | string_literal
      { Const (mkStringConst $1) (srclocOf $1) }
  | '(' expression_nlt ')'
      { $2 }
  | '(' expression_nlt error
      {% unclosed ($1 <--> $2) "(" }
  | '(' compound_statement ')'
      { StmExpr (mkBlockItems $2) ($1 `srcspan` $3) }
  | ANTI_ESC
      { AntiEscExp (getANTI_ESC $1) (srclocOf $1) }
  | ANTI_EXP
      { AntiExp (getANTI_EXP $1) (srclocOf $1) }

  -- Clang blocks
  | block_literal { $1 }

  -- Objective-C
  | objc_message_expression { $1 }
  | objc_at_expression      { $1 }

  -- CUDA -- C++11 lambda-expression subset
  | cuda_lambda_expression  { $1 }


postfix_expression :: { Exp }
postfix_expression :
    primary_expression
      { $1 }
  | postfix_expression '[' error
      {% unclosed (locOf $1) "[" }
  | postfix_expression '[' expression ']'
      { Index $1 $3 ($1 `srcspan` $4) }

  | postfix_expression '(' error
      {% unclosed (locOf $2) "(" }
  | postfix_expression '(' ')'
      { FnCall $1 [] ($1 `srcspan` $3) }
  | postfix_expression '(' argument_expression_list error
      {% unclosed ($2 <--> $3) "(" }
  | postfix_expression '(' argument_expression_list ')'
      { FnCall $1 $3 ($1 `srcspan` $4) }

  | postfix_expression '<<<' execution_configuration error
      {% unclosed ($2 <--> $3) "<<<" }
  | postfix_expression '<<<' execution_configuration '>>>' '(' ')'
      { CudaCall $1 $3 [] ($1 `srcspan` $6) }
  | postfix_expression '<<<' execution_configuration '>>>'
                       '(' argument_expression_list error
      {% unclosed ($5 <--> $6) "(" }
  | postfix_expression '<<<' execution_configuration '>>>'
                       '(' argument_expression_list ')'
      { CudaCall $1 $3 $6 ($1 `srcspan` $7) }

  | postfix_expression '.' identifier_or_typedef
      { Member $1 $3 ($1 `srcspan` $3) }
  | postfix_expression '->' identifier_or_typedef
      { PtrMember $1 $3 ($1 `srcspan` $3) }
  | postfix_expression '++'
      { PostInc $1 ($1 `srcspan` $2) }
  | postfix_expression '--'
      { PostDec $1 ($1 `srcspan` $2) }
  | '(' type_name ')' lbrace initializer_rlist '}'
      { CompoundLit ($2 :: Type) (rev $5) ($1 `srcspan` $6) }
  | '(' type_name ')' lbrace initializer_rlist ',' '}'
      { CompoundLit $2 (rev $5) ($1 `srcspan` $7) }

  -- GCC
  | '__builtin_va_arg' '(' assignment_expression ',' type_declaration ')'
      { BuiltinVaArg $3 $5 ($1 `srcspan` $6) }

unary_expression :: { Exp }
unary_expression :
    postfix_expression            { $1 }
  | '++' unary_expression         { PreInc $2 ($1 `srcspan` $2) }
  | '--' unary_expression         { PreDec $2 ($1 `srcspan` $2) }
  | '&' cast_expression           { UnOp AddrOf $2 ($1 `srcspan` $2) }
  | '*' cast_expression           { UnOp Deref $2 ($1 `srcspan` $2) }
  | '+' cast_expression           { UnOp Positive $2 ($1 `srcspan` $2) }
  | '-' cast_expression           { UnOp Negate $2 ($1 `srcspan` $2) }
  | '~' cast_expression           { UnOp Not $2 ($1 `srcspan` $2) }
  | '!' cast_expression           { UnOp Lnot $2 ($1 `srcspan` $2) }
  | 'sizeof' unary_expression     { SizeofExp $2 ($1 `srcspan` $2) }
  | 'sizeof' '(' type_name ')'    { SizeofType $3 ($1 `srcspan` $4) }
  | 'sizeof' '(' type_name error  {% unclosed ($2 <--> $3) "(" }

cast_expression :: { Exp }
cast_expression :
    unary_expression                   { $1 }
  | '(' type_name ')' cast_expression  { Cast $2 $4 ($1 `srcspan` $4) }
  | '(' type_name error                {% unclosed ($1 <--> $2) "(" }

multiplicative_expression :: { Exp }
multiplicative_expression :
    cast_expression
      { $1 }
  | multiplicative_expression '*' cast_expression
      { BinOp Mul $1 $3 ($1 `srcspan` $3) }
  | multiplicative_expression '/' cast_expression
      { BinOp Div $1 $3 ($1 `srcspan` $3) }
  | multiplicative_expression '%' cast_expression
      { BinOp Mod $1 $3 ($1 `srcspan` $3) }

additive_expression :: { Exp }
additive_expression :
    multiplicative_expression
      { $1 }
  | additive_expression '+' multiplicative_expression
      { BinOp Add $1 $3 ($1 `srcspan` $3) }
  | additive_expression '-' multiplicative_expression
      { BinOp Sub $1 $3 ($1 `srcspan` $3) }

shift_expression :: { Exp }
shift_expression :
    additive_expression
      { $1 }
  | shift_expression '<<' additive_expression
      { BinOp Lsh $1 $3 ($1 `srcspan` $3) }
  | shift_expression '>>' additive_expression
      { BinOp Rsh $1 $3 ($1 `srcspan` $3) }

relational_expression :: { Exp }
relational_expression :
    shift_expression
      { $1 }
  | relational_expression '<' shift_expression
      { BinOp Lt $1 $3 ($1 `srcspan` $3) }
  | relational_expression '>' shift_expression
      { BinOp Gt $1 $3 ($1 `srcspan` $3) }
  | relational_expression '<=' shift_expression
      { BinOp Le $1 $3 ($1 `srcspan` $3) }
  | relational_expression '>=' shift_expression
      { BinOp Ge $1 $3 ($1 `srcspan` $3) }

equality_expression :: { Exp }
equality_expression :
    relational_expression
      { $1 }
  | equality_expression '==' relational_expression
      { BinOp Eq $1 $3 ($1 `srcspan` $3) }
  | equality_expression '!=' relational_expression
      { BinOp Ne $1 $3 ($1 `srcspan` $3) }

and_expression :: { Exp }
and_expression :
    equality_expression
      { $1 }
  | and_expression '&' equality_expression
      { BinOp And $1 $3 ($1 `srcspan` $3) }

exclusive_or_expression :: { Exp }
exclusive_or_expression :
    and_expression
      { $1 }
  | exclusive_or_expression '^' and_expression
      { BinOp Xor $1 $3 ($1 `srcspan` $3) }

inclusive_or_expression :: { Exp }
inclusive_or_expression :
    exclusive_or_expression
      { $1 }
  | inclusive_or_expression '|' exclusive_or_expression
      { BinOp Or $1 $3 ($1 `srcspan` $3) }

logical_and_expression :: { Exp }
logical_and_expression :
    inclusive_or_expression
      { $1 }
  | logical_and_expression '&&' inclusive_or_expression
      { BinOp Land $1 $3 ($1 `srcspan` $3) }

logical_or_expression :: { Exp }
logical_or_expression :
    logical_and_expression
      { $1 }
  | logical_or_expression '||' logical_and_expression
      { BinOp Lor $1 $3 ($1 `srcspan` $3) }

conditional_expression :: { Exp }
conditional_expression :
    logical_or_expression
      { $1 }
  | logical_or_expression '?' expression ':' conditional_expression
      { Cond $1 $3 $5 ($1 `srcspan` $5) }

assignment_expression :: { Exp }
assignment_expression :
    conditional_expression
      { $1 }
  | unary_expression '=' assignment_expression
      { Assign $1 JustAssign $3 ($1 `srcspan` $3) }
  | unary_expression '*=' assignment_expression
      { Assign $1 MulAssign $3 ($1 `srcspan` $3) }
  | unary_expression '/=' assignment_expression
      { Assign $1 DivAssign $3 ($1 `srcspan` $3) }
  | unary_expression '%=' assignment_expression
      { Assign $1 ModAssign $3 ($1 `srcspan` $3) }
  | unary_expression '+=' assignment_expression
      { Assign $1 AddAssign $3 ($1 `srcspan` $3) }
  | unary_expression '-=' assignment_expression
      { Assign $1 SubAssign $3 ($1 `srcspan` $3) }
  | unary_expression '<<=' assignment_expression
      { Assign $1 LshAssign $3 ($1 `srcspan` $3) }
  | unary_expression '>>=' assignment_expression
      { Assign $1 RshAssign $3 ($1 `srcspan` $3) }
  | unary_expression '&=' assignment_expression
      { Assign $1 AndAssign $3 ($1 `srcspan` $3) }
  | unary_expression '^=' assignment_expression
      { Assign $1 XorAssign $3 ($1 `srcspan` $3) }
  | unary_expression '|=' assignment_expression
      { Assign $1 OrAssign $3 ($1 `srcspan` $3) }

expression :: { Exp }
expression :
    assignment_expression                 { $1 }
  | expression ',' assignment_expression  { Seq $1 $3 ($1 `srcspan` $3) }

maybe_expression :: { Maybe Exp  }
maybe_expression:
    {- empty -}  { Nothing }
  | expression   { Just $1 }

constant_expression :: { Exp }
constant_expression :
  conditional_expression { $1 }

argument_expression_list :: { [Exp] }
argument_expression_list :
    argument_expression_rlist { rev $1 }

argument_expression_rlist :: { RevList Exp }
argument_expression_rlist :
    assignment_expression
      { rsingleton $1 }
  | ANTI_ARGS
      { rsingleton (AntiArgs (getANTI_ARGS $1) (srclocOf $1)) }
  | argument_expression_rlist ',' assignment_expression
      { rcons $3 $1}
  | argument_expression_rlist ',' ANTI_ARGS
      { rcons (AntiArgs (getANTI_ARGS $3) (srclocOf $3)) $1 }

assignment_expression_list :: { [Exp] }
assignment_expression_list :
    assignment_expression_rlist { rev $1 }

assignment_expression_rlist :: { RevList Exp }
assignment_expression_rlist :
    assignment_expression
      { rsingleton $1 }
  | assignment_expression_rlist ',' assignment_expression
      { rcons $3 $1 }
  | assignment_expression_rlist ','
      { $1 }

{------------------------------------------------------------------------------
 -
 - _nlt expression variants
 -
 ------------------------------------------------------------------------------}

primary_expression_nlt :: { Exp }
primary_expression_nlt :
    identifier
      { Var $1 (srclocOf $1) }
  | constant
      { Const $1 (srclocOf $1) }
  | string_literal
      { Const (mkStringConst $1) (srclocOf $1) }
  | '(' expression_nlt ')'
      { $2 }
  | '(' expression_nlt error
      {% unclosed ($1 <--> $2) "(" }
  | '(' compound_statement ')'
      { StmExpr (mkBlockItems $2) ($1 `srcspan` $3) }
  | ANTI_ESC
      { AntiEscExp (getANTI_ESC $1) (srclocOf $1) }
  | ANTI_EXP
      { AntiExp (getANTI_EXP $1) (srclocOf $1) }

  -- Clang blocks
  | block_literal { $1 }

  -- Objective-C
  | objc_message_expression { $1 }
  | objc_at_expression      { $1 }

  -- CUDA -- C++11 lambda-expression subset
  | cuda_lambda_expression  { $1 }

postfix_expression_nlt :: { Exp }
postfix_expression_nlt :
    primary_expression_nlt
      { $1 }
  | postfix_expression_nlt '[' error
      {% unclosed (locOf $1) "[" }
  | postfix_expression_nlt '[' expression ']'
      { Index $1 $3 ($1 `srcspan` $4) }

  | postfix_expression_nlt '(' error
      {% unclosed (locOf $2) "(" }
  | postfix_expression_nlt '(' ')'
      { FnCall $1 [] ($1 `srcspan` $3) }
  | postfix_expression_nlt '(' argument_expression_nlt_list error
      {% unclosed ($2 <--> $3) "(" }
  | postfix_expression_nlt '(' argument_expression_nlt_list ')'
      { FnCall $1 $3 ($1 `srcspan` $4) }

  | postfix_expression_nlt '<<<' execution_configuration error
      {% unclosed ($2 <--> $3) "<<<" }
  | postfix_expression_nlt '<<<' execution_configuration '>>>' '(' ')'
      { CudaCall $1 $3 [] ($1 `srcspan` $6) }
  | postfix_expression_nlt '<<<' execution_configuration '>>>'
                       '(' argument_expression_nlt_list error
      {% unclosed ($5 <--> $6) "(" }
  | postfix_expression_nlt '<<<' execution_configuration '>>>'
                       '(' argument_expression_nlt_list ')'
      { CudaCall $1 $3 $6 ($1 `srcspan` $7) }

  | postfix_expression_nlt '.' identifier_or_typedef
      { Member $1 $3 ($1 `srcspan` $3) }
  | postfix_expression_nlt '->' identifier_or_typedef
      { PtrMember $1 $3 ($1 `srcspan` $3) }
  | postfix_expression_nlt '++'
      { PostInc $1 ($1 `srcspan` $2) }
  | postfix_expression_nlt '--'
      { PostDec $1 ($1 `srcspan` $2) }
  | '(' type_name ')' lbrace initializer_rlist '}'
      { CompoundLit ($2 :: Type) (rev $5) ($1 `srcspan` $6) }
  | '(' type_name ')' lbrace initializer_rlist ',' '}'
      { CompoundLit $2 (rev $5) ($1 `srcspan` $7) }

  -- GCC
  | '__builtin_va_arg' '(' assignment_expression_nlt ',' type_declaration ')'
      { BuiltinVaArg $3 $5 ($1 `srcspan` $6) }

unary_expression_nlt :: { Exp }
unary_expression_nlt :
    postfix_expression_nlt        { $1 }
  | '++' unary_expression         { PreInc $2 ($1 `srcspan` $2) }
  | '--' unary_expression         { PreDec $2 ($1 `srcspan` $2) }
  | '&' cast_expression           { UnOp AddrOf $2 ($1 `srcspan` $2) }
  | '*' cast_expression           { UnOp Deref $2 ($1 `srcspan` $2) }
  | '+' cast_expression           { UnOp Positive $2 ($1 `srcspan` $2) }
  | '-' cast_expression           { UnOp Negate $2 ($1 `srcspan` $2) }
  | '~' cast_expression           { UnOp Not $2 ($1 `srcspan` $2) }
  | '!' cast_expression           { UnOp Lnot $2 ($1 `srcspan` $2) }
  | 'sizeof' unary_expression     { SizeofExp $2 ($1 `srcspan` $2) }
  | 'sizeof' '(' type_name ')'    { SizeofType $3 ($1 `srcspan` $4) }
  | 'sizeof' '(' type_name error  {% unclosed ($2 <--> $3) "(" }

-- This lets us parse expressions like @foo = ...@ where @foo@ is a
-- typedef. Quite disgusting...
unary_expression_nlt_or_typedef :: { Exp }
unary_expression_nlt_or_typedef :
    unary_expression_nlt { $1 }
  | NAMED                { Var (Id (getNAMED $1) (srclocOf $1)) (srclocOf $1) }

cast_expression_nlt :: { Exp }
cast_expression_nlt :
    unary_expression_nlt               { $1 }
  | '(' type_name ')' cast_expression  { Cast $2 $4 ($1 `srcspan` $4) }
  | '(' type_name error                {% unclosed ($1 <--> $2) "(" }

multiplicative_expression_nlt :: { Exp }
multiplicative_expression_nlt :
    cast_expression_nlt
      { $1 }
  | multiplicative_expression_nlt '*' cast_expression
      { BinOp Mul $1 $3 ($1 `srcspan` $3) }
  | multiplicative_expression_nlt '/' cast_expression
      { BinOp Div $1 $3 ($1 `srcspan` $3) }
  | multiplicative_expression_nlt '%' cast_expression
      { BinOp Mod $1 $3 ($1 `srcspan` $3) }

additive_expression_nlt :: { Exp }
additive_expression_nlt :
    multiplicative_expression_nlt
      { $1 }
  | additive_expression_nlt '+' multiplicative_expression
      { BinOp Add $1 $3 ($1 `srcspan` $3) }
  | additive_expression_nlt '-' multiplicative_expression
      { BinOp Sub $1 $3 ($1 `srcspan` $3) }

shift_expression_nlt :: { Exp }
shift_expression_nlt :
    additive_expression_nlt
      { $1 }
  | shift_expression_nlt '<<' additive_expression
      { BinOp Lsh $1 $3 ($1 `srcspan` $3) }
  | shift_expression_nlt '>>' additive_expression
      { BinOp Rsh $1 $3 ($1 `srcspan` $3) }

relational_expression_nlt :: { Exp }
relational_expression_nlt :
    shift_expression_nlt
      { $1 }
  | relational_expression_nlt '<' shift_expression
      { BinOp Lt $1 $3 ($1 `srcspan` $3) }
  | relational_expression_nlt '>' shift_expression
      { BinOp Gt $1 $3 ($1 `srcspan` $3) }
  | relational_expression_nlt '<=' shift_expression
      { BinOp Le $1 $3 ($1 `srcspan` $3) }
  | relational_expression_nlt '>=' shift_expression
      { BinOp Ge $1 $3 ($1 `srcspan` $3) }

equality_expression_nlt :: { Exp }
equality_expression_nlt :
    relational_expression_nlt
      { $1 }
  | equality_expression_nlt '==' relational_expression
      { BinOp Eq $1 $3 ($1 `srcspan` $3) }
  | equality_expression_nlt '!=' relational_expression
      { BinOp Ne $1 $3 ($1 `srcspan` $3) }

and_expression_nlt :: { Exp }
and_expression_nlt :
    equality_expression_nlt
      { $1 }
  | and_expression_nlt '&' equality_expression
      { BinOp And $1 $3 ($1 `srcspan` $3) }

exclusive_or_expression_nlt :: { Exp }
exclusive_or_expression_nlt :
    and_expression_nlt
      { $1 }
  | exclusive_or_expression_nlt '^' and_expression_nlt
      { BinOp Xor $1 $3 ($1 `srcspan` $3) }

inclusive_or_expression_nlt :: { Exp }
inclusive_or_expression_nlt :
    exclusive_or_expression_nlt
      { $1 }
  | inclusive_or_expression_nlt '|' exclusive_or_expression
      { BinOp Or $1 $3 ($1 `srcspan` $3) }

logical_and_expression_nlt :: { Exp }
logical_and_expression_nlt :
    inclusive_or_expression_nlt
      { $1 }
  | logical_and_expression_nlt '&&' inclusive_or_expression
      { BinOp Land $1 $3 ($1 `srcspan` $3) }

logical_or_expression_nlt :: { Exp }
logical_or_expression_nlt :
    logical_and_expression_nlt
      { $1 }
  | logical_or_expression_nlt '||' logical_and_expression
      { BinOp Lor $1 $3 ($1 `srcspan` $3) }

conditional_expression_nlt :: { Exp }
conditional_expression_nlt :
    logical_or_expression_nlt
      { $1 }
  | logical_or_expression_nlt '?' expression ':' conditional_expression
      { Cond $1 $3 $5 ($1 `srcspan` $5) }

assignment_expression_nlt :: { Exp }
assignment_expression_nlt :
    conditional_expression_nlt
      { $1 }
  | unary_expression_nlt_or_typedef '=' assignment_expression
      { Assign $1 JustAssign $3 ($1 `srcspan` $3) }
  | unary_expression_nlt_or_typedef '*=' assignment_expression
      { Assign $1 MulAssign $3 ($1 `srcspan` $3) }
  | unary_expression_nlt_or_typedef '/=' assignment_expression
      { Assign $1 DivAssign $3 ($1 `srcspan` $3) }
  | unary_expression_nlt_or_typedef '%=' assignment_expression
      { Assign $1 ModAssign $3 ($1 `srcspan` $3) }
  | unary_expression_nlt_or_typedef '+=' assignment_expression
      { Assign $1 AddAssign $3 ($1 `srcspan` $3) }
  | unary_expression_nlt_or_typedef '-=' assignment_expression
      { Assign $1 SubAssign $3 ($1 `srcspan` $3) }
  | unary_expression_nlt_or_typedef '<<=' assignment_expression
      { Assign $1 LshAssign $3 ($1 `srcspan` $3) }
  | unary_expression_nlt_or_typedef '>>=' assignment_expression
      { Assign $1 RshAssign $3 ($1 `srcspan` $3) }
  | unary_expression_nlt_or_typedef '&=' assignment_expression
      { Assign $1 AndAssign $3 ($1 `srcspan` $3) }
  | unary_expression_nlt_or_typedef '^=' assignment_expression
      { Assign $1 XorAssign $3 ($1 `srcspan` $3) }
  | unary_expression_nlt_or_typedef '|=' assignment_expression
      { Assign $1 OrAssign $3 ($1 `srcspan` $3) }

expression_nlt :: { Exp }
expression_nlt :
    assignment_expression_nlt                 { $1 }
  | expression_nlt ',' assignment_expression  { Seq $1 $3 ($1 `srcspan` $3) }

maybe_expression_nlt :: { Maybe Exp  }
maybe_expression_nlt:
    {- empty -}  { Nothing }
  | expression_nlt   { Just $1 }

argument_expression_nlt_list :: { [Exp] }
argument_expression_nlt_list :
    assignment_expression_nlt
      { [$1] }
  | ANTI_ARGS
      { [AntiArgs (getANTI_ARGS $1) (srclocOf $1)] }
  | assignment_expression_nlt ',' argument_expression_rlist
      { $1 : rev $3 }
  | ANTI_ARGS ',' argument_expression_rlist
      { AntiArgs (getANTI_ARGS $1) (srclocOf $1) : rev $3 }

{------------------------------------------------------------------------------
 -
 - Declarations
 -
 ------------------------------------------------------------------------------}

{-
-- XXX: This is an awful hack to get around problems with the interaction
-- between lexer feedback and the one-token lookahead that happy does. If we
-- encounter a typedef and the next token is the newly typedef'd type, we get an
-- error if we include the terminal semicolon directly in the productions for
-- declaration. By splitting the semicolon out, the lookahead token is
-- guaranteed not to be a typedef use :)
-}

declaration :: { InitGroup }
declaration :
    declaration_ ';' { $1 }

declaration_nla :: { InitGroup }
declaration_nla :
    declaration_nla_ ';' { $1 }

declaration_ :: { InitGroup }
declaration_ :
    declaration_specifiers
      {% do{ let (dspec, decl)  = $1
           ; checkInitGroup dspec decl [] []
           }
      }
  | declaration_specifiers init_declarator_rlist
      {% do{ let (dspec, decl)  = $1
           ; let inits          = rev $2
           ; checkInitGroup dspec decl [] (rev $2)
           }
      }
  | declaration_specifiers error
      {% do{ let (_, decl)  = $1
           ; expected ["';'"] (Just "declaration")
           }
      }
  | ANTI_DECL
      { AntiDecl (getANTI_DECL $1) (srclocOf $1) }

declaration_nla_ :: { InitGroup }
declaration_nla_ :
    declaration_specifiers_nla
      {% do{ let (dspec, decl)  = $1
           ; checkInitGroup dspec decl [] []
           }
      }
  | declaration_specifiers_nla init_declarator_rlist
      {% do{ let (dspec, decl)  = $1
           ; let inits          = rev $2
           ; checkInitGroup dspec decl [] (rev $2)
           }
      }
  | declaration_specifiers_nla error
      {% do{ let (_, decl)  = $1
           ; expected ["';'"] (Just "declaration")
           }
      }
  | ANTI_DECL
      { AntiDecl (getANTI_DECL $1) (srclocOf $1) }

declaration_specifiers :: { (DeclSpec, Decl) }
declaration_specifiers :
    ANTI_TYPE
      { let  {  v  = getANTI_TYPE $1
             ;  l  = srclocOf $1
             }
        in
          (AntiTypeDeclSpec [] [] v l, AntiTypeDecl v l)
      }
  | storage_qualifier_specifiers ANTI_TYPE
      { let { storage   = mkStorage $1
            ; typeQuals = mkTypeQuals $1
            ; v         = getANTI_TYPE $2
            ; l         = $1 `srcspan` $2
            }
        in
          (AntiTypeDeclSpec storage typeQuals v l, AntiTypeDecl v l)
      }
  | nontypedef_declaration_specifiers
      { $1 }
  | typedef_declaration_specifiers
      { $1 }

declaration_specifiers_nla :: { (DeclSpec, Decl) }
declaration_specifiers_nla :
    ANTI_TYPE
      { let  {  v  = getANTI_TYPE $1
             ;  l  = srclocOf $1
             }
        in
          (AntiTypeDeclSpec [] [] v l, AntiTypeDecl v l)
      }
  | storage_qualifier_specifiers_nla ANTI_TYPE
      { let { storage   = mkStorage $1
            ; typeQuals = mkTypeQuals $1
            ; v         = getANTI_TYPE $2
            ; l         = $1 `srcspan` $2
            }
        in
          (AntiTypeDeclSpec storage typeQuals v l, AntiTypeDecl v l)
      }
  | nontypedef_declaration_specifiers_nla
      { $1 }
  | typedef_declaration_specifiers_nla
      { $1 }

nontypedef_declaration_specifiers :: { (DeclSpec, Decl) }
nontypedef_declaration_specifiers :
    ANTI_SPEC
      { let dspec = AntiDeclSpec (getANTI_SPEC $1) (srclocOf $1)
        in
          (dspec, DeclRoot (srclocOf $1))
      }
  | storage_qualifier_specifiers %prec NAMED
      {% do{ dspec <- mkDeclSpec $1
           ; return (dspec, DeclRoot (srclocOf $1))
           }
      }
  | type_specifier
      {% do{ dspec <- mkDeclSpec [$1]
           ; return (dspec, DeclRoot (srclocOf $1) )
           }
      }
  | type_specifier declaration_specifiers_rlist
      {% do{ dspec <- mkDeclSpec ($1 : rev $2)
           ; return (dspec, DeclRoot ($1 `srcspan` $2))
           }
      }
  | storage_qualifier_specifiers type_specifier
      {% do{ dspec <- mkDeclSpec ($1 ++ [$2])
           ; return $(dspec, DeclRoot ($1 `srcspan` $2))
           }
      }
  | storage_qualifier_specifiers type_specifier declaration_specifiers_rlist
      {% do{ dspec <- mkDeclSpec ($1 ++ $2 : rev $3)
           ; return (dspec, DeclRoot ($1 `srcspan` $3))
           }
      }

nontypedef_declaration_specifiers_nla :: { (DeclSpec, Decl) }
nontypedef_declaration_specifiers_nla :
    ANTI_SPEC
      { let dspec = AntiDeclSpec (getANTI_SPEC $1) (srclocOf $1)
        in
          (dspec, DeclRoot (srclocOf $1))
      }
  | storage_qualifier_specifiers_nla %prec NAMED
      {% do{ dspec <- mkDeclSpec $1
           ; return (dspec, DeclRoot (srclocOf $1))
           }
      }
  | type_specifier
      {% do{ dspec <- mkDeclSpec [$1]
           ; return (dspec, DeclRoot (srclocOf $1) )
           }
      }
  | type_specifier declaration_specifiers_rlist
      {% do{ dspec <- mkDeclSpec ($1 : rev $2)
           ; return (dspec, DeclRoot ($1 `srcspan` $2))
           }
      }
  | storage_qualifier_specifiers_nla type_specifier
      {% do{ dspec <- mkDeclSpec ($1 ++ [$2])
           ; return $(dspec, DeclRoot ($1 `srcspan` $2))
           }
      }
  | storage_qualifier_specifiers_nla type_specifier declaration_specifiers_rlist
      {% do{ dspec <- mkDeclSpec ($1 ++ $2 : rev $3)
           ; return (dspec, DeclRoot ($1 `srcspan` $3))
           }
      }

typedef_declaration_specifiers :: { (DeclSpec, Decl) }
typedef_declaration_specifiers :
    typedef_name
      {% do{ dspec <- mkDeclSpec [$1]
           ; return (dspec, DeclRoot (srclocOf $1))
           }
      }
  | typedef_name storage_qualifier_specifiers
      {% do{ dspec <- mkDeclSpec ($1 : $2)
           ; return (dspec, DeclRoot ($1 `srcspan` $2))
           }
      }
  | storage_qualifier_specifiers typedef_name
      {% do{ dspec <- mkDeclSpec ($1 ++ [$2])
           ; return (dspec, DeclRoot ($1 `srcspan` $2))
           }
      }
  | storage_qualifier_specifiers typedef_name storage_qualifier_specifiers
      {% do{ dspec <- mkDeclSpec ($1 ++ $2 : $3)
           ; return (dspec, DeclRoot ($1 `srcspan` $3))
           }
      }

typedef_declaration_specifiers_nla :: { (DeclSpec, Decl) }
typedef_declaration_specifiers_nla :
    typedef_name
      {% do{ dspec <- mkDeclSpec [$1]
           ; return (dspec, DeclRoot (srclocOf $1))
           }
      }
  | typedef_name storage_qualifier_specifiers
      {% do{ dspec <- mkDeclSpec ($1 : $2)
           ; return (dspec, DeclRoot ($1 `srcspan` $2))
           }
      }
  | storage_qualifier_specifiers_nla typedef_name
      {% do{ dspec <- mkDeclSpec ($1 ++ [$2])
           ; return (dspec, DeclRoot ($1 `srcspan` $2))
           }
      }
  | storage_qualifier_specifiers_nla typedef_name storage_qualifier_specifiers
      {% do{ dspec <- mkDeclSpec ($1 ++ $2 : $3)
           ; return (dspec, DeclRoot ($1 `srcspan` $3))
           }
      }

declaration_specifiers_rlist :: { RevList TySpec }
declaration_specifiers_rlist :
    storage_class_specifier                              { rsingleton $1 }
  | type_specifier                                       { rsingleton $1 }
  | type_qualifier                                       { rsingleton $1 }
  | attribute_specifier                                  { rapp (map TSAttr $1) rnil }
  | declaration_specifiers_rlist storage_class_specifier { rcons $2 $1 }
  | declaration_specifiers_rlist type_specifier          { rcons $2 $1 }
  | declaration_specifiers_rlist type_qualifier          { rcons $2 $1 }
  | declaration_specifiers_rlist attribute_specifier     { rapp (map TSAttr $2) $1 }

-- This production allows us to add storage class specifiers and type qualifiers
-- to an anti-quoted type.

storage_qualifier_specifiers :: { [TySpec]}
storage_qualifier_specifiers :
    storage_qualifier_specifiers_rlist { rev $1 }

storage_qualifier_specifiers_rlist :: { RevList TySpec }
storage_qualifier_specifiers_rlist :
    storage_class_specifier                                    { rsingleton $1 }
  | type_qualifier                                             { rsingleton $1 }
  | attribute_specifier                                        { rapp (map TSAttr $1) rnil }
  | storage_qualifier_specifiers_rlist storage_class_specifier { rcons $2 $1 }
  | storage_qualifier_specifiers_rlist type_qualifier          { rcons $2 $1 }
  | storage_qualifier_specifiers_rlist attribute_specifier     { rapp (map TSAttr $2) $1 }

storage_qualifier_specifiers_nla :: { [TySpec]}
storage_qualifier_specifiers_nla :
    storage_qualifier_specifiers_rlist_nla { rev $1 }

storage_qualifier_specifiers_rlist_nla :: { RevList TySpec }
storage_qualifier_specifiers_rlist_nla :
    storage_class_specifier                                        { rsingleton $1 }
  | type_qualifier                                                 { rsingleton $1 }
  | storage_qualifier_specifiers_rlist_nla storage_class_specifier { rcons $2 $1 }
  | storage_qualifier_specifiers_rlist_nla type_qualifier          { rcons $2 $1 }
  | storage_qualifier_specifiers_rlist_nla attribute_specifier     { rapp (map TSAttr $2) $1 }

init_declarator_rlist :: { RevList Init }
init_declarator_rlist :
    init_declarator                            { rsingleton $1 }
  | init_declarator_rlist ',' init_declarator  { rcons $3 $1 }

init_declarator :: { Init }
init_declarator :
    declarator attributes_and_label
      { let  {  (ident, declToDecl) = $1
             ;  decl                = declToDecl (declRoot ident)
             ;  (attrs, asmlabel)   = unLoc $2
             }
        in
          Init ident decl asmlabel Nothing attrs (ident `srcspan` $2)
      }
  | declarator attributes_and_label '=' initializer
      { let  {  (ident, declToDecl) = $1
             ;  decl                = declToDecl (declRoot ident)
             ;  (attrs, asmlabel)   = unLoc $2
             }
        in
          Init ident decl asmlabel (Just $4) attrs (ident `srcspan` $4)
      }
  | declarator error
      {% do{  let (ident, declToDecl) = $1
           ;  let decl                = declToDecl (declRoot ident)
           ;  expected ["'='"] Nothing
           }
      }

storage_class_specifier :: { TySpec }
storage_class_specifier :
    'auto'                  { TSauto (srclocOf $1) }
  | 'register'              { TSregister (srclocOf $1) }
  | 'static'                { TSstatic (srclocOf $1) }
  | 'extern'                { TSextern Nothing (srclocOf $1) }
  | 'extern' string_literal { TSextern (Just $2) ($1 `srcspan` $2) }
  | 'typedef'               { TStypedef (srclocOf $1) }

  -- Clang blocks
  | '__block'               { TS__block (srclocOf $1) }

  -- Objective-C
  | '__weak'                { TSObjC__weak (srclocOf $1) }
  | '__strong'              { TSObjC__strong (srclocOf $1) }
  | '__unsafe_unretained'   { TSObjC__unsafe_unretained (srclocOf $1) }

type_specifier :: { TySpec }
type_specifier :
    'void'                    { TSvoid (srclocOf $1) }
  | 'char'                    { TSchar (srclocOf $1) }
  | 'short'                   { TSshort (srclocOf $1) }
  | 'int'                     { TSint (srclocOf $1) }
  | 'long'                    { TSlong (srclocOf $1) }
  | 'float'                   { TSfloat (srclocOf $1) }
  | 'double'                  { TSdouble (srclocOf $1) }
  | 'signed'                  { TSsigned (srclocOf $1) }
  | 'unsigned'                { TSunsigned (srclocOf $1) }
  | struct_or_union_specifier { $1 }
  | enum_specifier            { $1 }

  -- C99
  | '_Bool'      { TS_Bool (srclocOf $1) }
  | '_Complex'   { TS_Complex (srclocOf $1) }
  | '_Imaginary' { TS_Imaginary (srclocOf $1) }

  -- GCC
  | '__builtin_va_list' { TSva_list (srclocOf $1) }

struct_or_union_specifier :: { TySpec }
struct_or_union_specifier :
    struct_or_union identifier_or_typedef
      { (unLoc $1) (Just $2) Nothing [] ($1 `srcspan` $2) }
  | struct_or_union attribute_specifiers identifier_or_typedef
      { (unLoc $1) (Just $3) Nothing $2 ($1 `srcspan` $3) }
  | struct_or_union lbrace struct_declaration_rlist '}'
      { (unLoc $1) Nothing (Just (rev $3)) [] ($1 `srcspan` $4) }
  | struct_or_union lbrace struct_declaration_rlist error
      {% unclosed ($1 <--> rev $3) "{" }
  | struct_or_union identifier_or_typedef lbrace struct_declaration_rlist '}'
      { (unLoc $1) (Just $2) (Just (rev $4)) [] ($1 `srcspan` $5) }
  | struct_or_union identifier_or_typedef lbrace struct_declaration_rlist error
      {% unclosed ($1 <--> rev $4) "{" }
  | struct_or_union attribute_specifiers identifier_or_typedef lbrace struct_declaration_rlist '}'
      { (unLoc $1) (Just $3) (Just (rev $5)) $2 ($1 `srcspan` $6) }
  | struct_or_union attribute_specifiers lbrace struct_declaration_rlist '}'
      { (unLoc $1) Nothing (Just (rev $4)) $2 ($1 `srcspan` $5) }
  | struct_or_union attribute_specifiers identifier_or_typedef lbrace struct_declaration_rlist error
      {% unclosed ($1 <--> rev $5) "{" }

struct_or_union :: { L (Maybe Id -> Maybe [FieldGroup] -> [Attr] -> SrcLoc -> TySpec) }
struct_or_union :
    'struct' { L (locOf $1) TSstruct }
  | 'union'  { L (locOf $1) TSunion }

struct_declaration_rlist :: { RevList FieldGroup }
struct_declaration_rlist :
    struct_declaration
      { rsingleton $1 }
  | ANTI_SDECLS
      { rsingleton (AntiSdecls (getANTI_SDECLS $1) (srclocOf $1)) }
  | struct_declaration_rlist struct_declaration
      { rcons $2 $1 }
  | struct_declaration_rlist ANTI_SDECLS
      { rcons (AntiSdecls (getANTI_SDECLS $2) (srclocOf $2)) $1 }

struct_declaration :: { FieldGroup }
struct_declaration :
    ANTI_SPEC struct_declarator_rlist semi
      { let dspec = AntiDeclSpec (getANTI_SPEC $1) (srclocOf $1)
        in
          FieldGroup dspec (map ($ Nothing) (rev $2)) ($1 `srcspan` $3)
      }
  | specifier_qualifier_list semi
      {%  do{ dspec <- mkDeclSpec $1
            ; gcc <- useGccExts
            ; c11 <- useC11Exts
            ; when (not gcc && not c11) $
                  expectedAt $2 ["declarator"] Nothing
            ; checkAnonymousStructOrUnion $2 dspec
            ; return $ FieldGroup dspec [] ($1 `srcspan` $2)
            }
      }
  | specifier_qualifier_list struct_declarator_rlist semi
      {%  do{ dspec <- mkDeclSpec $1
            ; return $ FieldGroup dspec (map ($ Nothing) (rev $2)) ($1 `srcspan` $3)
            }
      }
  | ANTI_TYPE struct_declarator_rlist semi
      {%  do{ let v     = getANTI_TYPE $1
            ; let dspec = AntiTypeDeclSpec [] [] v (srclocOf $1)
            ; let decl  = AntiTypeDecl v (srclocOf $1)
            ; return $ FieldGroup dspec (map ($ Just decl) (rev $2)) ($1 `srcspan` $3)
            }
      }
  | ANTI_SDECL
      { AntiSdecl (getANTI_SDECL $1) (srclocOf $1) }

specifier_qualifier_list :: { [TySpec] }
specifier_qualifier_list :
    type_specifier specifier_qualifier_rlist
      { $1 : rev $2 }
  | type_qualifier_rlist type_specifier specifier_qualifier_rlist
      { rev $1 ++ [$2] ++ rev $3 }
  | typedef_name
      { [$1] }
  | typedef_name type_qualifier_rlist
      { $1 : rev $2 }
  | type_qualifier_rlist typedef_name
      { rev $1 ++ [$2] }
  | type_qualifier_rlist typedef_name type_qualifier_rlist
      { rev $1 ++ [$2] ++ rev $3 }

specifier_qualifier_rlist :: { RevList TySpec }
specifier_qualifier_rlist :
    {- empty -}                                   { rnil }
  | specifier_qualifier_rlist type_specifier      { rcons $2 $1 }
  | specifier_qualifier_rlist type_qualifier      { rcons $2 $1 }
  | specifier_qualifier_rlist attribute_specifier { $1 }

struct_declarator_rlist :: { RevList (Maybe Decl -> Field) }
struct_declarator_rlist :
    struct_declarator                             { rsingleton $1 }
  | struct_declarator_rlist ',' struct_declarator { rcons $3 $1 }

struct_declarator :: { Maybe Decl -> Field }
struct_declarator :
    declarator
        { \maybe_decl ->
              let { (ident, declToDecl) = $1
                  ; decl                = declToDecl (fromMaybe (declRoot ident) maybe_decl)
                  }
              in
                Field (Just ident) (Just decl) Nothing (srclocOf decl)
        }
  | declarator attribute_specifiers
        { \maybe_decl ->
              let { (ident, declToDecl) = $1
                  ; decl                = declToDecl (fromMaybe (declRoot ident) maybe_decl)
                  }
              in
                Field (Just ident) (Just decl) Nothing (srclocOf decl)
        }
  | ':' constant_expression
        { \maybe_decl -> Field Nothing maybe_decl (Just $2) ($1 `srcspan` $2) }
  | declarator ':' constant_expression
        { \maybe_decl ->
              let { (ident, declToDecl) = $1
                  ; decl                = declToDecl (fromMaybe (declRoot ident) maybe_decl)
                  }
              in
                Field (Just ident) (Just decl) (Just $3) (srclocOf decl)
        }

enum_specifier :: { TySpec }
enum_specifier :
    'enum' identifier_or_typedef
      { TSenum (Just $2) [] [] ($1 `srcspan` $2) }
  | 'enum' attribute_specifiers identifier_or_typedef
      { TSenum (Just $3) [] $2 ($1 `srcspan` $3) }
  | 'enum' lbrace enumerator_rlist '}'
      { TSenum Nothing (rev $3) [] ($1 `srcspan` $4) }
  | 'enum' identifier_or_typedef lbrace enumerator_rlist '}'
      { TSenum (Just $2) (rev $4) [] ($1 `srcspan` $5)}

enumerator_rlist :: { RevList CEnum }
enumerator_rlist :
    enumerator
      { rsingleton $1 }
  | ANTI_ENUMS
      { rsingleton (AntiEnums (getANTI_ENUMS $1) (srclocOf $1)) }
  | enumerator_rlist ','
      { $1 }
  | enumerator_rlist ',' enumerator
      { rcons $3 $1 }
  | enumerator_rlist ',' ANTI_ENUMS
      { rcons (AntiEnums (getANTI_ENUMS $3) (srclocOf $3)) $1 }

enumerator :: { CEnum }
enumerator:
    identifier
      { CEnum $1 Nothing (srclocOf $1)}
  | identifier '=' constant_expression
      { CEnum $1 (Just $3) ($1 `srcspan` $3) }
  | ANTI_ENUM
      { AntiEnum (getANTI_ENUM $1) (srclocOf $1) }

type_qualifier :: { TySpec }
type_qualifier :
    'const'    { TSconst (srclocOf $1) }
  | 'volatile' { TSvolatile (srclocOf $1) }

  | ANTI_TYPE_QUAL  { TSAntiTypeQual (getANTI_TYPE_QUAL $1) (srclocOf $1) }
  | ANTI_TYPE_QUALS { TSAntiTypeQuals (getANTI_TYPE_QUALS $1) (srclocOf $1) }

  -- C99
  | 'inline'   { TSinline (srclocOf $1) }
  | 'restrict' { TSrestrict (srclocOf $1) }

  -- GCC
  | '__restrict' { TS__restrict (srclocOf $1) }

  -- CUDA
  | '__device__'   { TSCUDAdevice (srclocOf $1) }
  | '__global__'   { TSCUDAglobal (srclocOf $1) }
  | '__host__'     { TSCUDAhost (srclocOf $1) }
  | '__constant__' { TSCUDAconstant (srclocOf $1) }
  | '__shared__'   { TSCUDAshared (srclocOf $1) }
  | '__restrict__' { TSCUDArestrict (srclocOf $1) }
  | '__noinline__' { TSCUDAnoinline (srclocOf $1) }

  -- OpenCL
  | 'private'      { TSCLprivate (srclocOf $1) }
  | '__private'    { TSCLprivate (srclocOf $1) }
  | 'local'        { TSCLlocal (srclocOf $1) }
  | '__local'      { TSCLlocal (srclocOf $1) }
  | 'global'       { TSCLglobal (srclocOf $1) }
  | '__global'     { TSCLglobal (srclocOf $1) }
  | 'constant'     { TSCLconstant (srclocOf $1) }
  | '__constant'   { TSCLconstant (srclocOf $1) }
  | 'read_only'    { TSCLreadonly (srclocOf $1) }
  | '__read_only'  { TSCLreadonly (srclocOf $1) }
  | 'write_only'   { TSCLwriteonly (srclocOf $1) }
  | '__write_only' { TSCLwriteonly (srclocOf $1) }
  | 'kernel'       { TSCLkernel (srclocOf $1) }
  | '__kernel'     { TSCLkernel (srclocOf $1) }

-- Consider the following C program:
--
-- typedef struct foo {
--     int a;
-- } foo;
--
-- void f(foo* (foo));
--
-- In the grammar in the C99 standard, a parameter declaration can result from
-- either a declarator or an abstract declarator. This produces an ambiguity
-- when a typedef name appears after '(' because we can't tell whether or not it
-- is an item in a parameter list for a function that is part of an abstract
-- declarator, or if is just a parenthesized (standard) declarator. This
-- ambiguity exists in the definition of f in the above program.
--
-- To solve this ambiguity, we split the the declarator rule to handle the
-- identifier and typedef name cases separately, and, furthermore, copy the
-- typedef name declarator rule and remove the cases that leads to ambiguity
-- when a declarator is used in a parameter list declaration.

identifier_declarator :: { (Id, Decl -> Decl) }
identifier_declarator :
    identifier_direct_declarator
      { $1 }
  | pointer identifier_direct_declarator
      { let (ident, dirDecl) = $2
        in
          (ident, dirDecl . $1)
      }

identifier_direct_declarator :: { (Id, Decl -> Decl) }
identifier_direct_declarator :
    identifier
      { ($1, id) }
  | '(' identifier_declarator ')'
      { $2 }
  | '(' identifier_declarator error
      {%do  {  let (ident, declToDecl) = $2
            ;  let decl                = declToDecl (declRoot ident)
            ;  unclosed ($1 <--> decl) "("
            }
      }
  | identifier_direct_declarator array_declarator
      { let (ident, declToDecl) = $1
        in
           (ident, declToDecl . $2)
      }
  | identifier_direct_declarator '(' ')'
      { let { (ident, declToDecl) = $1
            ; proto = mkOldProto []
            }
        in
          (ident, declToDecl . proto)
      }
  | identifier_direct_declarator '(' parameter_type_list ')'
      { let { (ident, declToDecl) = $1
            ; proto = mkProto $3
            }
        in
          (ident, declToDecl . proto)
      }
  | identifier_direct_declarator '(' identifier_rlist ')'
      { let { (ident, declToDecl) = $1
            ; proto = mkOldProto (rev $3)
            }
        in
          (ident, declToDecl . proto)
      }

typedef_declarator :: { (Id, Decl -> Decl) }
typedef_declarator :
    typedef_direct_declarator
      { $1 }
  | pointer typedef_direct_declarator
      { let (ident, dirDecl) = $2
        in
          (ident, dirDecl . $1)
      }

typedef_direct_declarator :: { (Id, Decl -> Decl) }
typedef_direct_declarator :
    NAMED
      { (Id (getNAMED $1) (srclocOf $1), id) }
  | '(' typedef_declarator ')'
      { $2 }
  | '(' typedef_declarator error
      {%do  {  let (ident, declToDecl) = $2
            ;  let decl                = declToDecl (declRoot ident)
            ;  unclosed ($1 <--> decl) "("
            }
      }
  | typedef_direct_declarator array_declarator
      { let (ident, declToDecl) = $1
        in
           (ident, declToDecl . $2)
      }
  | typedef_direct_declarator '(' ')'
      { let { (ident, declToDecl) = $1
            ; proto = mkOldProto []
            }
        in
          (ident, declToDecl . proto)
      }
  | typedef_direct_declarator '(' parameter_type_list ')'
      { let { (ident, declToDecl) = $1
            ; proto = mkProto $3
            }
        in
          (ident, declToDecl . proto)
      }
  | typedef_direct_declarator '(' identifier_rlist ')'
      { let { (ident, declToDecl) = $1
            ; proto = mkOldProto (rev $3)
            }
        in
          (ident, declToDecl . proto)
      }

  -- Objective-C
  | OBJCNAMED
      { (Id (getOBJCNAMED $1) (srclocOf $1), id) }

declarator :: { (Id, Decl -> Decl) }
declarator :
    identifier_declarator
      { $1 }
  | typedef_declarator
      { $1 }

parameter_typedef_declarator :: { (Id, Decl -> Decl) }
parameter_typedef_declarator :
    parameter_typedef_direct_declarator
      { $1 }
  | pointer parameter_typedef_direct_declarator
      { let (ident, dirDecl) = $2
        in
          (ident, dirDecl . $1)
      }

parameter_typedef_direct_declarator :: { (Id, Decl -> Decl) }
parameter_typedef_direct_declarator :
    NAMED
      { (Id (getNAMED $1) (srclocOf $1), id) }
  | '(' pointer parameter_typedef_direct_declarator ')'
      { let (ident, dirDecl) = $3
        in
          (ident, dirDecl . $2)
      }
  | parameter_typedef_direct_declarator array_declarator
      { let (ident, declToDecl) = $1
        in
           (ident, declToDecl . $2)
      }
  | parameter_typedef_direct_declarator '(' ')'
      { let { (ident, declToDecl) = $1
            ; proto = mkOldProto []
            }
        in
          (ident, declToDecl . proto)
      }
  | parameter_typedef_direct_declarator '(' parameter_type_list ')'
      { let { (ident, declToDecl) = $1
            ; proto = mkProto $3
            }
        in
          (ident, declToDecl . proto)
      }
  | parameter_typedef_direct_declarator '(' identifier_rlist ')'
      { let { (ident, declToDecl) = $1
            ; proto = mkOldProto (rev $3)
            }
        in
          (ident, declToDecl . proto)
      }

  -- Objective-C
  | OBJCNAMED
      { (Id (getOBJCNAMED $1) (srclocOf $1), id) }

parameter_declarator :: { (Id, Decl -> Decl) }
parameter_declarator :
    identifier_declarator
      { $1 }
  | parameter_typedef_declarator
      { $1 }

array_declarator :: { Decl -> Decl }
array_declarator :
    '[' ']'
      { mkArray [] (NoArraySize ($1 `srcspan` $2)) }
  | '[' error
      {% unclosed (locOf $1) "[" }
  | '[' type_qualifier_rlist ']'
      { mkArray (rev $2) (NoArraySize ($1 `srcspan` $3)) }
  | '[' assignment_expression ']'
      { mkArray [] (ArraySize False $2 (srclocOf $2)) }
  | '[' type_qualifier_rlist assignment_expression ']'
      { mkArray (rev $2) (ArraySize False $3 (srclocOf $3)) }
  | '[' 'static' assignment_expression ']'
      { mkArray [] (ArraySize True $3 (srclocOf $3)) }
  | '[' 'static' type_qualifier_rlist assignment_expression ']'
      { mkArray (rev $3) (ArraySize True $4 (srclocOf $4)) }
  | '[' type_qualifier_rlist 'static' assignment_expression ']'
      { mkArray (rev $2) (ArraySize True $4 (srclocOf $4)) }
  | '[' '*' ']'
      { mkArray [] (VariableArraySize ($1 `srcspan` $3)) }
  | '[' type_qualifier_rlist  '*' ']'
      { mkArray (rev $2) (VariableArraySize ($1 `srcspan` $4)) }

-- Extension: blocks <http://clang.llvm.org/docs/BlockLanguageSpec.html>
--
-- Any declarator for a function pointer turns into a block declarator by replacing the '*' by a '^'.
-- However, block pointers can only point to function types.
--
pointer :: { Decl -> Decl }
pointer :
    '*'                              { mkPtr [] }
  | '*' type_qualifier_rlist         { mkPtr (rev $2) }
  | '*' pointer                      { $2 . mkPtr [] }
  | '*' type_qualifier_rlist pointer { $3 . mkPtr (rev $2) }

  -- Clang blocks
  | '^'                              {% mkBlockPtr (locOf $1) [] }
  | '^' type_qualifier_rlist         {% mkBlockPtr (locOf $1) (rev $2) }
  | '^' pointer                      {% ($2 .) `liftM` mkBlockPtr (locOf $1) [] }
  | '^' type_qualifier_rlist pointer {% ($3 .) `liftM` mkBlockPtr (locOf $1) (rev $2) }

type_qualifier_list :: { [TypeQual] }
type_qualifier_list :
    type_qualifier_rlist { mkTypeQuals (rev $1) }

type_qualifier_rlist :: { RevList TySpec }
type_qualifier_rlist :
    type_qualifier                      { rsingleton $1 }
  | type_qualifier_rlist type_qualifier { rcons $2 $1 }

parameter_type_list :: { Params }
parameter_type_list :
    parameter_rlist
      { let params = rev $1
        in
          Params params False (srclocOf params)
      }
  | parameter_rlist ',' '...'
      { let params = rev $1
        in
          Params params True (params `srcspan` $3)
      }

parameter_list :: { [Param] }
parameter_list :
    parameter_rlist { rev $1 }

parameter_rlist :: { RevList Param }
parameter_rlist :
    parameter_declaration
      { rsingleton $1 }
  | ANTI_PARAMS
      { rsingleton (AntiParams (getANTI_PARAMS $1) (srclocOf $1)) }
  | parameter_rlist ',' parameter_declaration
      { rcons $3 $1 }
  | parameter_rlist ',' ANTI_PARAMS
      { rcons (AntiParams (getANTI_PARAMS $3) (srclocOf $3))  $1 }

parameter_declaration :: { Param }
parameter_declaration :
    declaration_specifiers
      { let (dspec, decl) = $1
        in
          Param Nothing dspec decl (dspec `srcspan` decl)
      }
  | declaration_specifiers parameter_declarator
      { let  {  (dspec, declRoot)   = $1
             ;  (ident, declToDecl) = $2
             ;  decl                = declToDecl declRoot
             }
        in
          Param (Just ident) dspec decl (ident `srcspan` decl)
      }
  | declaration_specifiers abstract_declarator
      { let  {  (dspec, declRoot)  = $1
             ;  decl               = $2 declRoot
             }
        in
          Param Nothing dspec decl (dspec `srcspan` decl)
      }
  | ANTI_PARAM
      { AntiParam (getANTI_PARAM $1) (srclocOf $1) }

-- The type_declaration rule is the parameter_declaration rule without the
-- ANTI_PARAM option. This allows us to parse type declarations easily for later
-- antiquoting.

type_declaration :: { Type }
type_declaration :
    declaration_specifiers
      { let (dspec, decl) = $1
        in
          Type dspec decl (dspec `srcspan` decl)
      }
  | declaration_specifiers parameter_declarator
      { let  {  (dspec, declRoot)   = $1
             ;  (ident, declToDecl) = $2
             ;  decl                = declToDecl declRoot
             }
        in
          Type dspec decl (dspec `srcspan` decl)
      }
  | declaration_specifiers abstract_declarator
      { let  {  (dspec, declRoot)  = $1
             ;  decl               = $2 declRoot
             }
        in
          Type dspec decl (dspec `srcspan` decl)
      }

identifier_rlist :: { RevList Id }
identifier_rlist :
    identifier                       { rsingleton $1 }
  | identifier_rlist ',' identifier  { rcons $3 $1 }

type_name :: { Type }
type_name :
    ANTI_SPEC
      { let dspec = AntiDeclSpec (getANTI_SPEC $1) (srclocOf $1)
        in
          Type dspec (declRoot $1) (srclocOf $1)
      }
  | specifier_qualifier_list
      {% do{ dspec <- mkDeclSpec $1
           ; return $ Type dspec (declRoot $1) (srclocOf $1)
           }
      }
  | ANTI_SPEC abstract_declarator
      { let { dspec = AntiDeclSpec (getANTI_SPEC $1) (srclocOf $1)
            ; decl = $2 (declRoot $1)
            }
        in
          Type dspec decl (dspec `srcspan` decl)
      }
  | specifier_qualifier_list abstract_declarator
      {% do{ let decl = $2 (declRoot $1)
           ; dspec <- mkDeclSpec $1
           ; return $ Type dspec decl (dspec `srcspan` decl)
           }
      }
  | ANTI_TYPE
      { AntiType (getANTI_TYPE $1) (srclocOf $1) }
  | type_qualifier_list ANTI_TYPE
      { let  {  v     = getANTI_TYPE $2
             ;  decl  = declRoot (AntiTypeDecl v (srclocOf $2))
             }
        in
          Type (AntiTypeDeclSpec [] $1 v (srclocOf $2)) decl ($1 `srcspan` decl)
      }
  | ANTI_TYPE abstract_declarator
      { let  {  v     = getANTI_TYPE $1
             ;  decl  = $2 (AntiTypeDecl v (srclocOf $1))
             }
        in
          Type (AntiTypeDeclSpec [] [] v (srclocOf $1)) decl ($1 `srcspan` decl)
      }
  | type_qualifier_list ANTI_TYPE abstract_declarator
      { let  {  v     = getANTI_TYPE $2
             ;  decl  = $3 (AntiTypeDecl v (srclocOf $2))
             }
        in
          Type (AntiTypeDeclSpec [] $1 v (srclocOf $2)) decl ($1 `srcspan` decl)
      }

abstract_declarator :: { Decl -> Decl }
abstract_declarator :
    pointer                             { $1 }
  | direct_abstract_declarator          { $1 }
  | pointer direct_abstract_declarator  { $2 . $1 }

direct_abstract_declarator :: { Decl -> Decl }
direct_abstract_declarator :
    '(' abstract_declarator ')'
      { $2 }
  | '(' abstract_declarator error
      {% do{ let decl = $2 (declRoot $1)
           ; unclosed ($1 <--> decl) "("
           }
      }
  | array_declarator
      { $1 }
  | direct_abstract_declarator array_declarator
      { $1 . $2 }
  | '(' ')'
      { mkOldProto [] }
  | '(' parameter_type_list ')'
      { mkProto $2 }
  | direct_abstract_declarator '(' ')'
      { $1 . mkOldProto [] }
  | direct_abstract_declarator '(' parameter_type_list ')'
      { $1 . mkProto $3 }

typedef_name :: { TySpec }
typedef_name :
    NAMED
      { TSnamed (Id (getNAMED $1) (srclocOf $1)) [] (srclocOf $1) }
  | NAMED '<' identifier_rlist '>'
      {% do { assertObjCEnabled ($1 <--> $4) "To use protocol qualifiers, enable support for Objective-C"
            ; return $ TSnamed (Id (getNAMED $1) (srclocOf $1)) (rev $3) ($1 `srcspan` $4)
            } }
  | 'typename' identifier
      { TSnamed $2 [] (srclocOf $1) }
  | 'typename' identifier '<' identifier_rlist '>'
      {% do { assertObjCEnabled ($1 <--> $5) "To use protocol qualifiers, enable support for Objective-C"
            ; return $ TSnamed $2 (rev $4) ($1 `srcspan` $5)
            } }
  | 'typename' error
      {% expected ["identifier"] (Just "'typename'")}

  -- GCC
  | '__typeof__' '(' unary_expression_nlt ')'
      { TStypeofExp $3 ($1 `srcspan` $4) }
  | '__typeof__' '(' type_name ')'
      { TStypeofType $3 ($1 `srcspan` $4) }
  | '__typeof__' '(' type_name error
      {% unclosed ($2 <--> $3) "(" }

  -- Objective-C
  | OBJCNAMED
      { TSnamed (Id (getOBJCNAMED $1) (srclocOf $1)) [] (srclocOf $1) }
  | OBJCNAMED '<' identifier_rlist '>'
      {% do { assertObjCEnabled ($1 <--> $4) "To use protocol qualifiers, enable support for Objective-C"
            ; return $ TSnamed (Id (getOBJCNAMED $1) (srclocOf $1)) (rev $3) ($1 `srcspan` $4)
            }
      }

initializer :: { Initializer }
initializer :
    assignment_expression
      { ExpInitializer $1 (srclocOf $1) }
  | lbrace initializer_rlist '}'
      { CompoundInitializer (rev $2) ($1 `srcspan` $3) }
  | lbrace initializer_rlist error
      {% do{  let (_, inits) = unzip (rev $2)
           ;  unclosed ($1 <--> inits) "{"
           }
      }
  | lbrace initializer_rlist ',' '}'
      { CompoundInitializer (rev $2) ($1 `srcspan` $4) }
  | lbrace initializer_rlist ',' error
      {% unclosed ($1 <--> $3) "{" }
  | ANTI_INIT
      { AntiInit (getANTI_INIT $1) (srclocOf $1) }

initializer_rlist :: { RevList (Maybe Designation, Initializer) }
initializer_rlist :
    initializer
      { rsingleton (Nothing, $1) }
  | ANTI_INITS
      { rsingleton (Nothing, AntiInits (getANTI_INITS $1) (srclocOf $1)) }
  | designation initializer
      { rsingleton (Just $1, $2) }
  | initializer_rlist ',' initializer
      { rcons (Nothing, $3) $1 }
  | initializer_rlist ',' designation initializer
      { rcons (Just $3, $4) $1 }

designation :: { Designation }
designation :
    designator_rlist '='
      { let designators = rev $1
        in
          Designation designators (designators `srcspan` $2)
      }

designator_rlist :: { RevList Designator }
designator_rlist :
    designator                  { rsingleton $1 }
  | designator_rlist designator { rcons $2 $1 }

designator :: { Designator }
designator :
    '[' constant_expression ']'
      { IndexDesignator $2 ($1 `srcspan` $3) }
  | '.' identifier
      { MemberDesignator $2 ($1 `srcspan` $2) }

{------------------------------------------------------------------------------
 -
 - Statements
 -
 ------------------------------------------------------------------------------}

statement :: { Stm }
statement :
    labeled_statement      { $1 }
  | compound_statement     { $1 }
  | expression_statement   { $1 }
  | selection_statement    { $1 }
  | iteration_statement    { $1 }
  | jump_statement         { $1 }
  | '#pragma'              { Pragma (getPRAGMA $1) (srclocOf $1) }
  | comment statement      { $1 $2 }
  | ANTI_ESCSTM            { AntiEscStm (getANTI_ESCSTM $1) (srclocOf $1) }
  | ANTI_COMMENT error     {% expected ["statement"] Nothing }
  | ANTI_PRAGMA            { AntiPragma (getANTI_PRAGMA $1) (srclocOf $1) }
  | ANTI_STM               { AntiStm (getANTI_STM $1) (srclocOf $1) }

  -- GCC
  | asm_statement        { $1 }

  -- Objective-C
  | objc_at_statement    { $1 }

comment :: { Stm -> Stm }
comment :
     '//'         { mkCommentStm $1 }
  |  ANTI_COMMENT { \stm -> AntiComment (getANTI_COMMENT $1) stm (srclocOf $1) }

statement_list :: { [Stm] }
statement_list :
    comment                 { [$1 (Exp Nothing noLoc)] }
  | statement_rlist         { rev $1 }
  | statement_rlist comment { rev (rcons ($2 (Exp Nothing noLoc)) $1) }

statement_rlist :: { RevList Stm }
statement_rlist :
     statement
       { rsingleton $1 }
  |  ANTI_STMS
       { rsingleton (AntiStms (getANTI_STMS $1) (srclocOf $1)) }
  |  comment ANTI_STMS
       { AntiStms (getANTI_STMS $2) (srclocOf $2) `rcons` $1 (Exp Nothing noLoc) `rcons` rnil }
  |  statement_rlist statement
       { $2 `rcons` $1 }
  |  statement_rlist ANTI_STMS
       { AntiStms (getANTI_STMS $2) (srclocOf $2) `rcons` $1 }
  |  statement_rlist comment ANTI_STMS
       { AntiStms (getANTI_STMS $3) (srclocOf $3) `rcons` $2 (Exp Nothing noLoc) `rcons` $1 }

labeled_statement :: { Stm }
labeled_statement :
    identifier ':' error                      {% expected ["statement"] (Just "label") }
  | identifier ':' statement                  { Label $1 [] $3 ($1 `srcspan` $3) }
  | 'case' constant_expression error
    {% do { gcc_enabled <- useGccExts
          ; let options = if gcc_enabled then ["`:'", "`...'"] else ["`:'"]
          ; expected options Nothing } }
  | 'case' constant_expression ':' error      {% expected ["statement"] Nothing }
  | 'case' constant_expression ':' statement  { Case $2 $4 ($1 `srcspan` $4) }
  | 'case' constant_expression '...' constant_expression error
    {% expected ["`:'"] Nothing }
  | 'case' constant_expression '...' constant_expression ':' error
    {% expected ["statement"] Nothing }
  | 'case' constant_expression '...' constant_expression ':' statement
    {% gccOnly "To use ranges in case statements, enable GCC extensions"
        $ CaseRange $2 $4 $6 ($1 `srcspan` $6) }
  | 'default' error                           {% expected ["`:'"] (Just "`default'")}
  | 'default' ':' error                       {% expected ["statement"] Nothing }
  | 'default' ':' statement                   { Default $3 ($1 `srcspan` $3) }

  -- GCC
  | identifier ':' attribute_specifiers ';'   { Label $1 $3 (Exp Nothing noLoc) ($1 `srcspan` $4) }

compound_statement :: { Stm }
compound_statement:
    '{' begin_scope end_scope '}'
      { mkBlock [] ($1 `srcspan` $4) }
  | '{' begin_scope block_item_list end_scope '}'
      { mkBlock $3 ($1 `srcspan` $5) }
  | '{' begin_scope error
      {% unclosed (locOf $3) "{" }

block_item_list :: { [BlockItem] }
block_item_list :
     block_item_rlist         { rev $1 }
  |  block_item_rlist comment { rev (rcons (BlockStm ($2 (Exp Nothing noLoc))) $1) }

block_item_rlist :: { RevList BlockItem }
block_item_rlist :
     block_item_no_stm                          { rsingleton $1 }
  |  comment block_item_no_stm                  { rsingleton $2 }
  |  statement                                  { rsingleton (BlockStm $1) }
  |  block_item_rlist block_item_no_stm         { rcons $2 $1 }
  |  block_item_rlist comment block_item_no_stm { rcons $3 $1 }
  |  block_item_rlist statement                 { rcons (BlockStm $2) $1 }

block_item_no_stm  :: { BlockItem }
block_item_no_stm :
     declaration { BlockDecl $1 }
  |  ANTI_DECLS  { BlockDecl (AntiDecls (getANTI_DECLS $1) (srclocOf $1)) }
  |  ANTI_STMS   { BlockStm (AntiStms (getANTI_STMS $1) (srclocOf $1)) }
  |  ANTI_ITEM   { AntiBlockItem (getANTI_ITEM $1) (srclocOf $1) }
  |  ANTI_ITEMS  { AntiBlockItems (getANTI_ITEMS $1)  (srclocOf $1) }

block_item  :: { BlockItem }
block_item :
     statement                 { BlockStm $1 }
  |  statement comment         { BlockStm $1 }
  |  block_item_no_stm         { $1 }
  |  comment block_item_no_stm { $2 }
  |  block_item_no_stm comment { $1 }

begin_scope :: { () }
begin_scope : {% pushScope }

end_scope :: { () }
end_scope : {% popScope }

expression_statement :: { Stm }
expression_statement:
    ';'                  { Exp Nothing (srclocOf $1) }
  | expression_nlt ';'   { Exp (Just $1) ($1 `srcspan` $2) }
  | expression_nlt error {% expected ["';'"] Nothing }

selection_statement :: { Stm }
selection_statement :
    'if' '(' expression ')' statement
      { If $3 $5 Nothing ($1 `srcspan` $5) }
  | 'if' '(' expression ')' statement 'else' statement
      { If $3 $5 (Just $7) ($1 `srcspan` $7) }
  | 'if' error
      {% expected ["("] (Just "`if'") }
  | 'if' '(' expression error
      {% unclosed ($2 <--> $3) "(" }
  | 'switch' '(' expression ')' statement
      { Switch $3 $5 ($1 `srcspan` $5) }
  | 'switch' '(' expression error
      {% unclosed ($2 <--> $3) "(" }

iteration_statement :: { Stm }
iteration_statement :
    'while' '(' expression ')' statement
      { While $3 $5 ($1 `srcspan` $5) }
  | 'while' '(' expression error
      {% unclosed ($2 <--> $3) "(" }
  | 'do' statement 'while' '(' expression ')' ';'
      { DoWhile $2 $5 ($1 `srcspan` $7) }
  | 'do' statement 'while' '(' expression error
      {% unclosed ($4 <--> $5) "(" }
  | 'for' '(' error
      {% expected ["expression", "declaration"] Nothing }
  | 'for' '(' declaration maybe_expression semi ')' statement
      { For (Left $3) $4 Nothing $7 ($1 `srcspan` $7) }
  | 'for' '(' maybe_expression_nlt semi maybe_expression semi ')' statement
      { For (Right $3) $5 Nothing $8 ($1 `srcspan` $8) }
  | 'for' '(' maybe_expression_nlt semi maybe_expression semi error
      {% unclosed ($2 <--> $6) "(" }
  | 'for' '(' declaration maybe_expression semi expression ')' statement
      { For (Left $3) $4 (Just $6) $8 ($1 `srcspan` $8) }
  | 'for' '(' maybe_expression_nlt semi maybe_expression semi expression ')' statement
      { For (Right $3) $5 (Just $7) $9 ($1 `srcspan` $9) }
  | 'for' '(' maybe_expression_nlt semi maybe_expression semi expression error
      {% unclosed ($2 <--> $7) "(" }

jump_statement :: { Stm }
jump_statement :
    'goto' identifier ';'      { Goto $2 ($1 `srcspan` $3) }
  | 'goto' error               {% expected ["identifier"] (Just "`goto'") }
  | 'goto' identifier error    {% expected ["';'"] Nothing }
  | 'continue' ';'             { Continue ($1 `srcspan` $2) }
  | 'continue' error           {% expected ["';'"] (Just "`continue'") }
  | 'break' ';'                { Break ($1 `srcspan` $2) }
  | 'break' error              {% expected ["';'"] (Just "`break'") }
  | 'return' ';'               { Return Nothing ($1 `srcspan` $2) }
  | 'return' error             {% expected ["';'", "expression"] Nothing }
  | 'return' expression ';'    { Return (Just $2) ($1 `srcspan` $3) }
  | 'return' expression error  {% expected ["';'"] Nothing }

{------------------------------------------------------------------------------
 -
 - External definitions
 -
 ------------------------------------------------------------------------------}

translation_unit :: { [Definition] }
translation_unit :
    translation_unit_rlist { rev $1 }

translation_unit_rlist :: { RevList Definition }
translation_unit_rlist :
    {- empty -}
      { rnil }
  | comment
      { rnil }
  | translation_unit_rlist external_declaration
      { rcons $2 $1 }
  | translation_unit_rlist ANTI_EDECLS
      { rcons (AntiEdecls (getANTI_EDECLS $2) (srclocOf $2)) $1 }

external_declaration :: { Definition }
external_declaration :
    external_declaration_
      { $1 }
  | external_declaration_ comment
      { $1 }

  -- Objective-C
  | objc_class_declaration    { $1 }
  | objc_interface            { $1 }
  | objc_protocol_declaration { $1 }
  | objc_implementation       { $1 }
  | objc_compatibility_alias  { $1 }

external_declaration_ :: { Definition }
external_declaration_ :
    function_definition
      { FuncDef $1 (srclocOf $1) }
  | declaration
      { DecDef $1 (srclocOf $1) }
  | ANTI_FUNC
      { AntiFunc (getANTI_FUNC $1) (srclocOf $1) }
  | ANTI_ESC
      { AntiEsc (getANTI_ESC $1) (srclocOf $1) }
  | ANTI_EDECL
      { AntiEdecl (getANTI_EDECL $1) (srclocOf $1) }

function_definition :: { Func }
function_definition :
    declaration_specifiers declarator compound_statement
      {% do{ let (dspec, declRoot)   =  $1
           ; let (ident, declToDecl) =  $2
           ; let blockItems          =  mkBlockItems $3
           ; let decl                =  declToDecl declRoot
           ; case decl of
               { Proto protoDecl args _ -> return $
                     Func dspec ident protoDecl args
                          blockItems
                          (decl `srcspan` blockItems)
               ; OldProto protoDecl args _ -> return $
                     OldFunc dspec ident protoDecl args Nothing
                             blockItems
                             (decl `srcspan` blockItems)
               ; _ -> parserError (decl <--> blockItems)
                                  (text "bad function declaration")
               }
           }
      }
  | declaration_specifiers declarator declaration_rlist compound_statement
      {% do{ let (dspec, declRoot)   =  $1
           ; let (ident, declToDecl) =  $2
           ; let argDecls            =  $3
           ; let blockItems          =  mkBlockItems $4
           ; let decl                =  declToDecl declRoot
           ; case decl of
               { OldProto protoDecl args _ -> return $
                     OldFunc dspec ident protoDecl args (Just (rev argDecls))
                             blockItems
                             (decl `srcspan` blockItems)
               ; _ -> parserError (decl <--> blockItems)
                                  (text "bad function declaration")
               }
           }
      }

-- To prevent ambiguity, the first declaration in a list of old-style function
-- parameter declarations cannot start with an attribute.
declaration_rlist :: { RevList InitGroup }
declaration_rlist :
    declaration_nla
      { rsingleton $1 }
  | declaration_nla comment
      { rsingleton $1 }
  | ANTI_DECLS
      { rsingleton (AntiDecls (getANTI_DECLS $1) (srclocOf $1)) }
  | declaration_rlist declaration
      { rcons $2 $1 }
  | declaration_rlist declaration comment
      { rcons $2 $1 }
  | declaration_rlist ANTI_DECLS
      { rcons (AntiDecls (getANTI_DECLS $2) (srclocOf $2)) $1 }

{------------------------------------------------------------------------------
 -
 - GCC extensions
 -
 ------------------------------------------------------------------------------}

attributes_and_label :: { L ([Attr], Maybe AsmLabel) }
attributes_and_label :
    {- empty -}
      { L noLoc ([], Nothing) }
  | asmlabel
      { L (locOf $1) ([], Just $1) }
  | asmlabel attribute_specifiers
      { L ($1 <--> $2) ($2, Just $1) }
  | attribute_specifiers
      { L (locOf $1) ($1, Nothing) }
  | attribute_specifiers asmlabel
      { L ($1 <--> $2) ($1, Just $2) }
  | attribute_specifiers asmlabel attribute_specifiers
      { L ($1 <--> $3) ($1 ++ $3, Just $2) }

asmlabel :: { AsmLabel }
asmlabel :
    '__asm__' '(' error
      {% expected ["string literal"] Nothing }
  | '__asm__' '(' string_literal ')'
      { $3 }

attribute_specifiers_opt :: { [Attr] }
attribute_specifiers_opt :
    {- empty -}
      { [] }
  | attribute_specifiers
      { $1 }

attribute_specifiers :: { [Attr] }
attribute_specifiers :
    attribute_specifier                      { $1 }
  | attribute_specifiers attribute_specifier { $1 ++ $2 }

attribute_specifier :: { [Attr] }
attribute_specifier :
    '__attribute__' '(' '(' attribute_rlist ')' ')' { rev $4 }

attribute_rlist :: { RevList Attr }
attribute_rlist :
    attrib                     { rsingleton $1 }
  | attribute_rlist ',' attrib { rcons $3 $1 }
  | ANTI_ATTRS
     { rsingleton $ AntiAttrs (getANTI_ATTRS $1) (srclocOf $1) }

attrib :: { Attr }
attrib :
    attrib_name
      { Attr $1 [] (srclocOf $1)}
  | attrib_name '(' argument_expression_list ')'
      { Attr $1 $3 ($1 `srcspan` $4) }
  | ANTI_ATTR
     { AntiAttr (getANTI_ATTR $1) (srclocOf $1) }

attrib_name :: { Id }
attrib_name :
    identifier_or_typedef { $1 }
  | 'static'              { Id "static" (srclocOf $1) }
  | 'extern'              { Id "extern" (srclocOf $1) }
  | 'register'            { Id "register" (srclocOf $1) }
  | '__block'             { Id "__block" (srclocOf $1) }
  | 'typedef'             { Id "typedef" (srclocOf $1) }
  | 'inline'              { Id "inline" (srclocOf $1) }
  | 'auto'                { Id "auto" (srclocOf $1) }
  | 'const'               { Id "const" (srclocOf $1) }
  | 'volatile'            { Id "volatile" (srclocOf $1) }
  | 'unsigned'            { Id "unsigned" (srclocOf $1) }
  | 'long'                { Id "long" (srclocOf $1) }
  | 'short'               { Id "short" (srclocOf $1) }
  | 'signed'              { Id "signed" (srclocOf $1) }
  | 'int'                 { Id "int" (srclocOf $1) }
  | 'char'                { Id "char" (srclocOf $1) }
  | 'float'               { Id "float" (srclocOf $1) }
  | 'double'              { Id "double" (srclocOf $1) }
  | 'void'                { Id "void" (srclocOf $1) }

maybe_volatile :: { Bool }
maybe_volatile :
     {- empty -} { False}
  |  'volatile'  { True}

asm_statement :: { Stm }
asm_statement :
    '__asm__' maybe_volatile '(' string_literal ')' ';'
      { Asm $2 [] $4 [] [] [] ($1 `srcspan` $6) }
  | '__asm__' maybe_volatile '(' string_literal ':' asm_outs ')' ';'
      { Asm $2 [] $4 $6 [] [] ($1 `srcspan` $8) }
  | '__asm__' maybe_volatile '(' string_literal ':' asm_outs
                                                ':' asm_ins ')' ';'
      { Asm $2 [] $4 $6 $8 [] ($1 `srcspan` $10) }
  | '__asm__' maybe_volatile '(' string_literal ':' asm_outs
                                                ':' asm_ins
                                                ':' asm_clobbers ')' ';'
      { Asm $2 [] $4 $6 $8 $10 ($1 `srcspan` $12) }
  | '__asm__' maybe_volatile 'goto' '(' string_literal ':'
                                                       ':' asm_ins
                                                       ':' asm_clobbers
                                                       ':' asm_goto_labels ')' ';'
      { AsmGoto $2 [] $5 $8 $10 $12 ($1 `srcspan` $14) }

asm_ins :: { [AsmIn] }
asm_ins :
    {- empty -}   { [] }
  | asm_ins_rlist { rev $1 }

asm_ins_rlist :: { RevList AsmIn }
asm_ins_rlist :
    asm_in                   { rsingleton $1 }
  | asm_ins_rlist ',' asm_in { rcons $3 $1 }

asm_in :: { AsmIn }
asm_in :
    asm_symbolic_name_opt STRING '(' expression ')'
      { AsmIn $1 ((fst . getSTRING) $2) $4 }

asm_outs :: { [AsmOut] }
asm_outs :
    {- empty -}    { [] }
  | asm_outs_rlist { rev $1 }

asm_outs_rlist :: { RevList AsmOut }
asm_outs_rlist :
    asm_out                    { rsingleton $1 }
  | asm_outs_rlist ',' asm_out { rcons $3 $1 }

asm_out :: { AsmOut }
asm_out :
    asm_symbolic_name_opt STRING '(' identifier ')'
      { AsmOut $1 ((fst . getSTRING) $2) $4 }

asm_clobbers :: { [String] }
asm_clobbers :
    {- empty -}         { [] }
  | asm_clobbers_rlist  { rev $1 }

asm_clobbers_rlist :: { RevList String }
asm_clobbers_rlist :
    asm_clobber                        { rsingleton $1 }
  | asm_clobbers_rlist ',' asm_clobber { rcons $3 $1 }

asm_clobber :: { String }
asm_clobber :
    STRING { (fst . getSTRING) $1 }

asm_symbolic_name_opt :: { Maybe Id }
asm_symbolic_name_opt :
    {- empty -}        { Nothing }
  | '[' identifier ']' { Just $2 }

asm_goto_labels :: { [Id] }
asm_goto_labels :
    asm_goto_labels_rlist { rev $1 }

asm_goto_labels_rlist :: { RevList Id }
asm_goto_labels_rlist :
    identifier                           { rsingleton $1 }
  | asm_goto_labels_rlist ',' identifier { rcons $3 $1 }

{------------------------------------------------------------------------------
 -
 - Clang blocks
 -
 ------------------------------------------------------------------------------}

-- Clang extension: block literal expression
--
-- block-literal ->
--   '^' [block-type] attribute_specifiers_opt compound-statement
--
-- block-type ->
--   '(' parameter-list ')' | specifier-qualifier-list abstract-declarator
--
block_literal :: { Exp }
block_literal :
    '^'                                              attribute_specifiers_opt compound_statement
      {% do { assertBlocksEnabled ($1 <--> $3) "To use blocks, enable the blocks language extension"
            ; let items = mkBlockItems $3
            ; return $ BlockLit (BlockVoid (srclocOf $1)) $2 items ($1 `srcspan` $3)
            }
      }
  | '^' '(' parameter_list ')'                       attribute_specifiers_opt compound_statement
      {% do { assertBlocksEnabled ($1 <--> $6) "To use blocks, enable blocks language extension"
            ; let items = mkBlockItems $6
            ; return $ BlockLit (BlockParam $3 ($2 `srcspan` $4)) $5 items ($1 `srcspan` $6)
            }
      }
  | '^' specifier_qualifier_list abstract_declarator attribute_specifiers_opt compound_statement
      {% do { assertBlocksEnabled ($1 <--> $5) "To use blocks, enable blocks language extension"
            ; let decl    =  $3 (declRoot $2)
            ; let items   =  mkBlockItems $5
            ; dspec       <- mkDeclSpec $2
            ; let typeLoc =  dspec `srcspan` decl
            ; return $ BlockLit (BlockType (Type dspec decl typeLoc) typeLoc) $4 items ($1 `srcspan` $5)
            }
      }

{------------------------------------------------------------------------------
 -
 - Objective-C
 -
 ------------------------------------------------------------------------------}

objc_key_value :: { ObjCDictElem }
objc_key_value :
    assignment_expression ':' assignment_expression
      { ObjCDictElem $1 $3 ($1 `srcspan` $3) }

objc_key_value_rlist :: { RevList ObjCDictElem }
objc_key_value_rlist :
    objc_key_value
      { rsingleton $1 }
  | ANTI_OBJC_DICTS
      { rsingleton (AntiObjCDictElems (getANTI_OBJC_DICTS $1) (srclocOf $1)) }
  | objc_key_value_rlist ',' objc_key_value
      { rcons $3 $1 }

objc_string_literal_rlist :: { RevList Const }
objc_string_literal_rlist :
    '@' string_literal
      { rsingleton (mkStringConst $2) }
  | objc_string_literal_rlist '@' string_literal
      { rcons (mkStringConst $3) $1 }

objc_selector_rlist :: { RevList Id }
objc_selector_rlist :
    ':'
      { rsingleton (Id "" (srclocOf $1)) }
  | objc_selector ':'
      { rsingleton $1 }
  | objc_selector_rlist ':'
      { rcons (Id "" (srclocOf $2)) $1 }
  | objc_selector_rlist objc_selector ':'
      { rcons $2 $1 }

-- Objective-C extension: at statement
--
-- objc-at-statement ->
--     '@' 'try' compound-statement objc-catch-statement+
--   | '@' 'try' compound-statement objc-catch-statement* '@' 'finally' compound-statement
--   | '@' 'throw' [expression] ';'
--   | '@' 'synchronized' '(' expression ')' compound-statement
--   | '@' 'autoreleasepool' compound-statement
--
-- objc-catch-statement ->
--   '@' 'catch' '(' (parameter-declaration | '...') ')' compound-statement
--
-- NB: If a try-catch statement without a finally clause is followed by another '@' statement,
--     we require a ';' after the try-catch statement. To avoid that, we would need a lookahead
--     of 2 (to see what special keyword comes after the '@'). In LALR(1), we get a shift-reduce
--     conflict that shifts to continue the try-catch statement.
--
objc_at_statement :: { Stm }
objc_at_statement :
    '@' 'try' compound_statement objc_catch_statement_rlist '@' 'finally' compound_statement
      { let { tryItems     = mkBlockItems $3
            ; finallyItems = mkBlockItems $7
            }
        in
         ObjCTry tryItems (rev $4) (Just finallyItems) ($1 `srcspan` $7)
      }
  | '@' 'try' compound_statement objc_catch_statement_rlist
      {% do { let { tryItems   = mkBlockItems $3
                  ; catchStmts = rev $4
                  }
            ; when (null catchStmts) $
                throw $ ParserException ($1 <--> $3) $
                  text "@try statement without @finally needs at least one @catch statement"
            ; return $ ObjCTry tryItems catchStmts Nothing ($1 `srcspan` catchStmts)
            } }
  | '@' 'try' compound_statement objc_catch_statement_rlist '@' error
      {% parserError ($1 <--> $5)
           (text $ "a @try-@catch statement without a @finally clause needs to be followed\n" ++
                   "by a semicolon if the next statement begins with a '@'")
      }
  | '@' 'throw' expression ';'
      { ObjCThrow (Just $3) ($1 `srcspan` $4) }
  | '@' 'throw' expression error
      {% expected ["';'"] Nothing }
  | '@' 'throw' ';'
      { ObjCThrow Nothing ($1 `srcspan` $3) }
  | '@' 'throw' error
      {% expected ["';'", "expression"] Nothing }
  | '@' 'synchronized' '(' expression ')' compound_statement
      { let items = mkBlockItems $6
        in
         ObjCSynchronized $4 items ($1 `srcspan` $6)
      }
  | '@' 'autoreleasepool' compound_statement
      { let items = mkBlockItems $3
        in
         ObjCAutoreleasepool items ($1 `srcspan` $3)
      }

objc_catch_statement_rlist :: { RevList ObjCCatch }
objc_catch_statement_rlist :
    {- empty -}
      { rnil }
  | objc_catch_statement_rlist '@' 'catch' '(' parameter_declaration ')' compound_statement
      { let items = mkBlockItems $7
        in
        rcons (ObjCCatch (Just $5) items ($2 `srcspan` $7)) $1 }
  | objc_catch_statement_rlist '@' 'catch' '(' '...' ')' compound_statement
      { let items = mkBlockItems $7
        in
        rcons (ObjCCatch Nothing items ($2 `srcspan` $7)) $1 }

-- Objective-C extension: message expression
--
-- objc-message-expr ->
--   '[' objc-receiver
--         ( objc-selector
--         | ([objc-selector] ':' assignment-expression)+  (',' assignment-expression)*
--         )
--   ']'
--
-- objc-receiver -> 'super' | expression | class-name | type-name
--
-- objc-selector is an identifier whose lexeme may also be that of the following keywords and type names:
--    asm auto bool break case char const continue default do double else enum
--    extern false float for goto if inline int long register restrict return
--    short signed sizeof static struct switch true try typedef type name
--    typeof union unsigned void volatile wchar_t while _Bool _Complex
--    _Imaginary __alignof
--
objc_message_expression :: { Exp }
objc_message_expression :
    '[' objc_receiver objc_message_args ']'
      {% do { assertObjCEnabled ($1 <--> $4) "To use a message expression, enable Objective-C support"
            ; let (args, vargs) = $3
            ; return $ ObjCMsg $2 args vargs ($1 `srcspan` $4)
            }
      }

objc_receiver :: { ObjCRecv }
objc_receiver :
    expression
      { case $1 of
          Var (Id "super" _) loc -> ObjCRecvSuper loc
          _                      -> ObjCRecvExp $1 (srclocOf $1)
      }
  | ANTI_OBJC_RECV
      { AntiObjCRecv (getANTI_OBJC_RECV $1) (srclocOf $1) }

objc_message_args :: { ([ObjCArg], [Exp]) }
objc_message_args :
    objc_selector
      { ([ObjCArg (Just $1) Nothing (srclocOf $1)], []) }
  | objc_keywordarg_rlist objc_vararg_rlist
      { (rev $1, rev $2) }

objc_selector :: { Id }
objc_selector :
    identifier_or_typedef { $1 }
--    | 'asm'                 { Id "asm" (srclocOf $1) }
    | 'auto'                { Id "auto" (srclocOf $1) }
--    | 'bool'                { Id "bool" (srclocOf $1) }
    | 'break'               { Id "break" (srclocOf $1) }
    | 'case'                { Id "case" (srclocOf $1) }
    | 'char'                { Id "char" (srclocOf $1) }
    | 'const'               { Id "const" (srclocOf $1) }
    | 'continue'            { Id "continue" (srclocOf $1) }
    | 'default'             { Id "default" (srclocOf $1) }
    | 'do'                  { Id "do" (srclocOf $1) }
    | 'double'              { Id "double" (srclocOf $1) }
    | 'else'                { Id "else" (srclocOf $1) }
    | 'enum'                { Id "enum" (srclocOf $1) }
    | 'extern'              { Id "extern" (srclocOf $1) }
--    | 'false'               { Id "false" (srclocOf $1) }
    | 'float'               { Id "float" (srclocOf $1) }
    | 'for'                 { Id "for" (srclocOf $1) }
    | 'goto'                { Id "goto" (srclocOf $1) }
    | 'if'                  { Id "if" (srclocOf $1) }
    | 'inline'              { Id "inline" (srclocOf $1) }
    | 'int'                 { Id "int" (srclocOf $1) }
    | 'long'                { Id "long" (srclocOf $1) }
    | 'register'            { Id "register" (srclocOf $1) }
    | 'restrict'            { Id "restrict" (srclocOf $1) }
    | 'return'              { Id "return" (srclocOf $1) }
    | 'short'               { Id "short" (srclocOf $1) }
    | 'signed'              { Id "signed" (srclocOf $1) }
    | 'sizeof'              { Id "sizeof" (srclocOf $1) }
    | 'static'              { Id "static" (srclocOf $1) }
    | 'struct'              { Id "struct" (srclocOf $1) }
    | 'switch'              { Id "switch" (srclocOf $1) }
--    | 'true'                { Id "true" (srclocOf $1) }
--    | 'try'                 { Id "try" (srclocOf $1) }
    | 'typedef'             { Id "typedef" (srclocOf $1) }
    | 'typename'            { Id "typename" (srclocOf $1) }
--    | 'typeof'              { Id "typeof" (srclocOf $1) }
    | 'union'               { Id "union" (srclocOf $1) }
    | 'unsigned'            { Id "unsigned" (srclocOf $1) }
    | 'void'                { Id "void" (srclocOf $1) }
    | 'volatile'            { Id "volatile" (srclocOf $1) }
--    | 'wchar_t'             { Id "wchar_t" (srclocOf $1) }
    | 'while'               { Id "while" (srclocOf $1) }
--    | '_Bool'               { Id "_Bool" (srclocOf $1) }
--    | '_Complex'            { Id "_Complex" (srclocOf $1) }
--    | '_Imaginary'          { Id "_Imaginary" (srclocOf $1) }
    | '__block'             { Id "__block" (srclocOf $1) }
    | '__weak'              { Id "__weak" (srclocOf $1) }
    | '__strong'            { Id "__strong" (srclocOf $1) }
    | '__unsafe_unretained' { Id "__unsafe_unretained" (srclocOf $1) }
--    | '__alignof'           { Id "__alignof" (srclocOf $1) }

objc_keywordarg_rlist :: { RevList ObjCArg }   -- will be non-empty
objc_keywordarg_rlist :
    objc_keywordarg
      { rsingleton $1 }
  | objc_keywordarg_rlist objc_keywordarg
      { $2 `rcons` $1 }
  | ANTI_OBJC_ARGS
      { rsingleton (AntiObjCArgs (getANTI_OBJC_ARGS $1) (srclocOf $1)) }

objc_keywordarg :: { ObjCArg }
objc_keywordarg :
    ':' assignment_expression
      { ObjCArg Nothing (Just $2) ($1 `srcspan` $2) }
  | objc_selector ':' assignment_expression
      { ObjCArg (Just $1) (Just $3) ($1 `srcspan` $3) }
  | ANTI_OBJC_ARG
      { AntiObjCArg (getANTI_OBJC_ARG $1) (srclocOf $1) }

objc_vararg_rlist :: { RevList Exp }   -- might be empty
objc_vararg_rlist :
    {- empty -}
      { rnil }
  | objc_vararg_rlist ',' assignment_expression
      { $3 `rcons` $1 }
  | ANTI_ARGS
      { rsingleton (AntiArgs (getANTI_ARGS $1) (srclocOf $1)) }

-- Objective-C extension: at expression
--
-- objc-at-expression ->
--     '@' ['+' | '-'] constant
--   | '@' string-literal
--   | '@' ('YES' | 'NO')
--   | '@' '[' [assignment-expression (',' assignment-expression)* [',']] ']'
--   | '@' '{' [objc-key-value (',' objc-key-value)* [',']] '}'
--   | '@' '(' assignment-expression ')'
--   | '@' 'encode' '(' type-name ')'
--   | '@' 'protocol' '(' identifier ')'
--   | '@' 'selector' '(' ( objc-selector | ([objc-selector] ':')+ ')'
--
-- objc-key-value ->
--   assignment-expression ':' assignment-expression
--
-- NB: We need to make 'NO' and 'YES' into special tokens. If we match on "'@' identifier" instead,
--     we get lots of shift-reduce conflicts as other special tokens that may appear behind a '@'
--     can also reduce to "identifier".
--
objc_at_expression :: { Exp }
objc_at_expression :
    '@' constant
      { ObjCLitConst Nothing $2 ($1 `srcspan` $2) }
  | '@' '+' constant
      { ObjCLitConst (Just Positive) $3 ($1 `srcspan` $3) }
  | '@' '-' constant
      { ObjCLitConst (Just Negate) $3 ($1 `srcspan` $3) }
  | objc_string_literal_rlist
      { let lits = rev $1 in ObjCLitString lits (head lits `srcspan` last lits) }
  | '@' 'NO'
      { ObjCLitBool False ($1 `srcspan` $2) }
  | '@' 'YES'
      { ObjCLitBool True ($1 `srcspan` $2) }
  | '@' '[' ']'
      { ObjCLitArray [] ($1 `srcspan` $3) }
  | '@' '[' assignment_expression_list ']'
      { ObjCLitArray $3 ($1 `srcspan` $4) }
  | '@' lbrace '}'
      { ObjCLitDict [] ($1 `srcspan` $3) }
  | '@' lbrace objc_key_value_rlist '}'
      { ObjCLitDict (rev $3) ($1 `srcspan` $4) }
  | '@' lbrace objc_key_value_rlist ',' '}'
      { ObjCLitDict (rev $3) ($1 `srcspan` $5) }
  | '@' '(' expression ')'
      { ObjCLitBoxed $3 ($1 `srcspan` $4) }
  | '@' 'encode' '(' type_name ')'
      { ObjCEncode $4 ($1 `srcspan` $5) }
  | '@' 'protocol' '(' identifier ')'
      { ObjCProtocol $4 ($1 `srcspan` $5) }
  | '@' 'selector' '(' objc_selector ')'
      { let Id str _ = $4
        in
          ObjCSelector str ($1 `srcspan` $5)
      }
  | '@' 'selector' '(' objc_selector_rlist ')'
      { let str = concat [s ++ ":" | Id s _ <- rev $4]
        in
          ObjCSelector str ($1 `srcspan` $5)
      }

-- Objective-C extension: class declaration
--
-- objc-class-declaration ->
--   '@' 'class' identifier+ ';'
--
objc_class_declaration :: { Definition }
objc_class_declaration :
    '@' 'class' identifier_rlist semi
      {% do { let idents = rev $3
            ; mapM addClassdefId idents
            ; return $ ObjCClassDec idents ($1 `srcspan` $4)
            }
      }

-- Objective-C extension: class or category interface
--
-- objc-interface ->
--   [attribute_specifiers] objc-class-interface | objc-category-interface
--
-- objc-class-interface ->
--   '@' 'interface' identifier [':' identifier]
--     [objc-protocol-refs]
--     [objc-class-instance-variables]
--     objc-interface-decl*
--   '@' 'end'
--
-- objc-category-interface ->
--   '@' 'interface' identifier '(' [identifier] ')'
--     [objc-protocol-refs]
--     [objc-class-instance-variables]
--     objc-interface-decl*
--   '@' 'end'
--
-- objc-protocol-refs ->
--   '<' identifier-list '>'
--
-- objc-class-instance-variables ->
--   '{' objc-instance-variable-decl* '}'
--
-- objc-instance-variable-decl ->
--   objc-visibility-spec | [objc-instance-variable-decl] ';'
--
-- objc-instance-variable-decl -> struct-declaration
--
-- objc-visibility-spec -> '@' 'private' | '@' 'public' | '@' 'protected' | '@' 'package'
--
-- objc-interface-decl ->
--   objc-property-decl | objc-method-requirement | objc-method-proto ';' | declaration | ';'
--
-- objc-property-decl ->
--   '@' 'property' [objc-property-attrs] struct-declaration ';'
--
-- objc-property-attrs ->
--   '(' objc-property-attribute (',' objc-property-attribute)* ')'
--
-- objc-property-attribute ->
--     'getter' '=' objc-selector
--   | 'setter' '=' objc-selector ':'
--   | 'readonly'
--   | 'readwrite'
--   | 'assign'
--   | 'retain'
--   | 'copy'
--   | 'nonatomic'
--   | 'atomic'
--   | 'strong'
--   | 'weak'
--   | 'unsafe_unretained'
--
-- objc-method-requirement -> '@' 'required' | '@' 'optional'
--
-- objc-method-proto ->
--   ('-' | '+') objc-method-decl [attribute_specifiers]
--
-- objc-method-decl ->
--   ['(' type-name ')'] [attribute_specifiers]
--     ( objc-selector
--     | ([objc-selector] ':' ['(' type-name ')'] [attribute_specifiers] identifier)+
--     ) [',' '...']
--
-- NB: We omit C-style parameters to methods as they don't appear to be current anymore.
--

-- To avoid a reduce/reduce conflict, we parse the attributes that can appear
-- before an interface declaration using the storage_qualifier_specifiers
-- non-terminal and check it for non-attributes in the body of the production.
objc_interface :: { Definition }
objc_interface :
    '@' 'interface' identifier objc_interface_body
      {% do { let (prot, vars, decls, loc) = $4
            ; addClassdefId $3
            ; return $ ObjCClassIface $3 Nothing prot vars decls [] ($1 `srcspan` loc)
            }
      }
  | storage_qualifier_specifiers '@' 'interface' identifier objc_interface_body
      {% do { let (prot, vars, decls, loc) = $5
            ; addClassdefId $4
            ; attrs <- checkOnlyAttributes $1
            ; return $ ObjCClassIface $4 Nothing prot vars decls attrs ($2 `srcspan` loc)
            }
      }
  | '@' 'interface' identifier ':' identifier_or_typedef objc_interface_body
      {% do { let (prot, vars, decls, loc) = $6
            ; addClassdefId $3
            ; return $ ObjCClassIface $3 (Just $5) prot vars decls [] ($1 `srcspan` loc)
            }
      }
  | storage_qualifier_specifiers '@' 'interface' identifier ':' identifier_or_typedef objc_interface_body
      {% do { let (prot, vars, decls, loc) = $7
            ; addClassdefId $4
            ; attrs <- checkOnlyAttributes $1
            ; return $ ObjCClassIface $4 (Just $6) prot vars decls attrs ($2 `srcspan` loc)
            }
      }
  | '@' 'interface' identifier_or_typedef '(' ')' objc_interface_body
      { let (prot, vars, decls, loc) = $6
        in
          ObjCCatIface $3 Nothing prot vars decls ($1 `srcspan` loc)
      }
  | '@' 'interface' identifier_or_typedef '(' identifier ')' objc_interface_body
      { let (prot, vars, decls, loc) = $7
        in
          ObjCCatIface $3 (Just $5) prot vars decls ($1 `srcspan` loc)
      }

objc_interface_body :: { ([Id], [ObjCIvarDecl], [ObjCIfaceDecl], Loc) }
objc_interface_body :
   objc_protocol_refs_rlist objc_class_instance_variables_rlist objc_interface_decl_rlist  '@' 'end'
      { ( rev $1, rev $2, rev $3, $4 <--> $5) }

objc_protocol_refs_rlist :: { RevList Id }
objc_protocol_refs_rlist :
    {- empty -}
      { rnil }
  | '<' identifier_rlist '>'
      { $2 }

objc_class_instance_variables_rlist :: { RevList ObjCIvarDecl }
objc_class_instance_variables_rlist :
    {- empty -}
      { rnil }
  | lbrace '}'
      { rnil }
  | lbrace objc_instance_variable_decl_rlist '}'
      { $2 }

objc_instance_variable_decl_rlist :: { RevList ObjCIvarDecl }
objc_instance_variable_decl_rlist :
    objc_visibility_spec
      { rsingleton (ObjCIvarVisi $1 (srclocOf $1)) }
  | semi
      { rnil }
  | struct_declaration semi
      { rsingleton (ObjCIvarDecl $1 (srclocOf $1)) }
  | objc_instance_variable_decl_rlist objc_visibility_spec
      { rcons (ObjCIvarVisi $2 (srclocOf $2)) $1 }
  | objc_instance_variable_decl_rlist semi
      { $1 }
  | objc_instance_variable_decl_rlist struct_declaration semi
      { rcons (ObjCIvarDecl $2 (srclocOf $2)) $1 }

objc_visibility_spec :: { ObjCVisibilitySpec }
objc_visibility_spec :
    '@' 'objc_private'
      { ObjCPrivate ($1 `srcspan` $2) }
  | '@' 'public'
      { ObjCPublic ($1 `srcspan` $2) }
  | '@' 'protected'
      { ObjCProtected ($1 `srcspan` $2) }
  | '@' 'package'
      { ObjCPackage ($1 `srcspan` $2) }

objc_interface_decl_list :: { [ObjCIfaceDecl] }
objc_interface_decl_list :
    objc_interface_decl_rlist
      { rev $1 }

objc_interface_decl_rlist :: { RevList ObjCIfaceDecl }
objc_interface_decl_rlist :
    {- empty -}
      { rnil }
  | objc_interface_decl_rlist semi
      { $1 }
  | objc_interface_decl_rlist objc_interface_decl
      { rcons $2 $1 }
  | objc_interface_decl_rlist ANTI_OBJC_IFDECLS
      { rcons (AntiObjCIfaceDecls (getANTI_OBJC_IFDECLS $2) (srclocOf $2)) $1 }
  | objc_interface_decl_rlist ANTI_OBJC_PROPS
      { rcons (AntiObjCProps (getANTI_OBJC_PROPS $2) (srclocOf $2)) $1 }

objc_interface_decl :: { ObjCIfaceDecl }
objc_interface_decl :
    objc_property_decl
      { $1 }
  | objc_method_requirement
      { ObjCIfaceReq $1 (srclocOf $1) }
  | objc_method_proto semi
      { ObjCIfaceMeth $1 (srclocOf $1) }
  | declaration
      { ObjCIfaceDecl $1 (srclocOf $1) }
  | ANTI_OBJC_IFDECL
      { AntiObjCIfaceDecl (getANTI_OBJC_IFDECL $1) (srclocOf $1) }

objc_property_decl :: { ObjCIfaceDecl }
objc_property_decl :
    '@' 'property' struct_declaration
      { ObjCIfaceProp [] $3 ($1 `srcspan` $3) }
  | '@' 'property' '(' objc_property_attr_rlist ')' struct_declaration
      { ObjCIfaceProp (rev $4) $6 ($1 `srcspan` $6) }
  | ANTI_OBJC_PROP
      { AntiObjCProp (getANTI_OBJC_PROP $1) (srclocOf $1) }

objc_property_attr_rlist :: { RevList ObjCPropAttr }
objc_property_attr_rlist :
    objc_property_attr
      { rsingleton $1 }
  | objc_property_attr_rlist ',' objc_property_attr
      { rcons $3 $1 }
  | ANTI_OBJC_PROP_ATTRS
      { rsingleton (AntiObjCAttrs (getANTI_OBJC_PROP_ATTRS $1) (srclocOf $1)) }

objc_property_attr :: { ObjCPropAttr }
objc_property_attr :
    identifier '=' objc_selector
      {% case $1 of
           { Id "getter" _ -> return $ ObjCGetter $3 ($1 `srcspan` $3)
           ; _             -> expectedObjCPropertyAttr (locOf $1)
           }
      }
  | identifier '=' objc_selector ':'
      {% case $1 of
           { Id "setter" _ -> return $ ObjCSetter $3 ($1 `srcspan` $4)
           ; _             -> expectedObjCPropertyAttr (locOf $1)
           }
      }
  | identifier
      {% case $1 of
           { Id "readonly" _          -> return $ ObjCReadonly (srclocOf $1)
           ; Id "readwrite" _         -> return $ ObjCReadwrite (srclocOf $1)
           ; Id "assign" _            -> return $ ObjCAssign (srclocOf $1)
           ; Id "retain" _            -> return $ ObjCRetain (srclocOf $1)
           ; Id "copy" _              -> return $ ObjCCopy (srclocOf $1)
           ; Id "nonatomic" _         -> return $ ObjCNonatomic (srclocOf $1)
           ; Id "atomic" _            -> return $ ObjCAtomic (srclocOf $1)
           ; Id "strong" _            -> return $ ObjCStrong (srclocOf $1)
           ; Id "weak" _              -> return $ ObjCWeak (srclocOf $1)
           ; Id "unsafe_unretained" _ -> return $ ObjCUnsafeUnretained (srclocOf $1)
           ; _                        -> expectedObjCPropertyAttr (locOf $1)
           }
      }
  | ANTI_OBJC_PROP_ATTR
     { AntiObjCAttr (getANTI_OBJC_PROP_ATTR $1) (srclocOf $1) }

objc_method_requirement :: { ObjCMethodReq }
objc_method_requirement :
    '@' 'required'
      { ObjCRequired ($1 `srcspan` $2) }
  | '@' 'optional'
      { ObjCOptional ($1 `srcspan` $2) }

objc_method_proto :: { ObjCMethodProto }
objc_method_proto :
    '-' objc_method_decl attribute_specifiers_opt
      { let (res, attrs, params, hasVargs) = $2
        in
          ObjCMethodProto False res attrs params hasVargs $3 ($1 `srcspan` $3)
      }
  | '+' objc_method_decl attribute_specifiers_opt
      { let (res, attrs, params, hasVargs) = $2
        in
          ObjCMethodProto True res attrs params hasVargs $3 ($1 `srcspan` $3)
      }
  | ANTI_OBJC_METHOD_PROTO
      { AntiObjCMethodProto (getANTI_OBJC_METHOD_PROTO $1) (srclocOf $1) }

objc_method_decl :: { (Maybe Type, [Attr], [ObjCParam], Bool) }
objc_method_decl :
    attribute_specifiers_opt objc_method_params
      { (Nothing, $1, $2, False) }
  | '(' type_name ')' attribute_specifiers_opt objc_method_params
      { (Just $2, $4, $5, False) }
  | attribute_specifiers_opt objc_method_params ',' '...'
      { (Nothing, $1, $2, True) }
  | '(' type_name ')' attribute_specifiers_opt objc_method_params ',' '...'
      { (Just $2, $4, $5, True) }

objc_method_params :: { [ObjCParam] }
objc_method_params :
    objc_selector
      { [ObjCParam (Just $1) Nothing [] Nothing (srclocOf $1)] }
  | objc_method_param_rlist
      { rev $1 }

objc_method_param_rlist :: { RevList ObjCParam }
objc_method_param_rlist :
    objc_method_param
      { rsingleton $1 }
  | objc_method_param_rlist objc_method_param
      { rcons $2 $1 }
  | ANTI_OBJC_PARAMS
      { rsingleton (AntiObjCParams (getANTI_OBJC_PARAMS $1) (srclocOf $1)) }

objc_method_param :: { ObjCParam }
objc_method_param :
    objc_selector ':' '(' type_name ')' attribute_specifiers_opt identifier
      { ObjCParam (Just $1) (Just $4) $6 (Just $7) ($1 `srcspan` $7) }
  |               ':' '(' type_name ')' attribute_specifiers_opt identifier
      { ObjCParam Nothing   (Just $3) $5 (Just $6) ($1 `srcspan` $6) }
  | objc_selector ':'                   attribute_specifiers_opt identifier
      { ObjCParam (Just $1) Nothing   $3 (Just $4) ($1 `srcspan` $4) }
  |               ':'                   attribute_specifiers_opt identifier
      { ObjCParam Nothing   Nothing   $2 (Just $3) ($1 `srcspan` $3) }
  |   ANTI_OBJC_PARAM
      { AntiObjCParam (getANTI_OBJC_PARAM $1) (srclocOf $1) }

-- Objective-C extension: protocol declaration
--
-- objc-protocol-declaration ->
--   objc-protocol-definition | objc-protocol-forward-reference
--
-- objc-protocol-definition ->
--   '@' 'protocol' identifier
--     [objc-protocol-refs]
--     objc-interface-decl*
--   '@' 'end'
--
-- objc-protocol-forward-reference ->
--   '@' 'protocol' identifier-list ';'
--
-- NB: "@protocol identifier ;" should be parsed as a 'objc-protocol-forward-reference', which means that
--     'objc-interface-decl-list' in 'objc-protocol-definition' may not start with a semicolon if the
--     'objc-protocol-refs' are empty.
--
--     We achieve this by factoring the common prefix into the non-terminal 'objc_protocol_prefix' to turn
--     the ambiguity into a shift-reduce conflict that is resolved by preferring shifting.
--
objc_protocol_declaration :: { Definition }
objc_protocol_declaration :
    objc_protocol_prefix objc_protocol_refs_rlist objc_interface_decl_rlist '@' 'end'
      { ObjCProtDef (fst $1) (rev $2) (rev $3) (snd $1 `srcspan` $5) }
  -- This rule wins the shift-reduce conflict
  | objc_protocol_prefix semi
      { ObjCProtDec [fst $1] (snd $1 `srcspan` $2) }
  | objc_protocol_prefix ',' identifier_rlist semi
      { ObjCProtDec (fst $1 : rev $3) (snd $1 `srcspan` $4) }

objc_protocol_prefix :: { (Id, Loc) }
objc_protocol_prefix :
  '@' 'protocol' identifier
    { ($3, locOf $1) }

-- Objective-C extension: class or category implementation
--
-- objc-implementation ->
--   objc-class-implementation | objc-category-implementation
--
-- objc-class-implementation ->
--   '@' 'implementation' identifier [':' identifier]
--     [objc-class-instance-variables]
--     objc-implementation-decl*
--   '@' end
--
-- objc-category-implementation ->
--   '@' 'implementation' identifier '(' identifier ')'
--     objc-implementation-decl*
--   '@' end
--
-- objc-implementation-decl ->
--   function-definition | declaration | property-synthesize | property-dynamic | objc-method-definition
--
-- property-synthesize ->
--   '@' 'synthesize' property-ivar (',' property-ivar)* ';'
--
-- property-dynamic ->
--   '@' 'dynamic' identifier (',' identifier)* ';'
--
-- property-ivar ->
--   identifier | identifier '=' identifier
--
-- objc-method-definition ->
--   objc-method-proto [';'] compound_statement
--
objc_implementation :: { Definition }
objc_implementation :
    '@' 'implementation' identifier_or_typedef ':' identifier_or_typedef objc_implementation_body_vars
      { let (ivars, defs, loc) = $6
        in
          ObjCClassImpl $3 (Just $5) ivars defs ($1 `srcspan` loc)
      }
  | '@' 'implementation' identifier_or_typedef                           objc_implementation_body_vars
      { let (ivars, defs, loc) = $4
        in
          ObjCClassImpl $3 Nothing   ivars defs ($1 `srcspan` loc)
      }
  | '@' 'implementation' identifier_or_typedef '(' identifier ')'        objc_implementation_body
      { ObjCCatImpl $3 $5 (fst $7) ($1 `srcspan` snd $7) }

objc_implementation_body_vars :: { ([ObjCIvarDecl], [Definition], Loc) }
objc_implementation_body_vars :
  objc_class_instance_variables_rlist objc_implementation_body
    { (rev $1, fst $2, snd $2) }

objc_implementation_body :: { ([Definition], Loc) }
objc_implementation_body :
  objc_implementation_decl_rlist '@' 'end'
    { (rev $1, locOf $3) }

objc_implementation_decl_list :: { [Definition] }
objc_implementation_decl_list :
    objc_implementation_decl_rlist
      { rev $1 }

objc_implementation_decl_rlist :: { RevList Definition }
objc_implementation_decl_rlist :
    {- empty -}
      { rnil }
  | objc_implementation_decl_rlist function_definition
      { rcons (FuncDef $2 (srclocOf $2)) $1 }
  | objc_implementation_decl_rlist declaration
      { rcons (DecDef $2 (srclocOf $2)) $1 }
  | objc_implementation_decl_rlist property_synthesize
      { rcons $2 $1 }
  | objc_implementation_decl_rlist property_dynamic
      { rcons $2 $1 }
  | objc_implementation_decl_rlist objc_method_definition
      { rcons $2 $1 }
  | objc_implementation_decl_rlist ANTI_FUNC
      { rcons (AntiFunc (getANTI_FUNC $2) (srclocOf $2)) $1 }
  | objc_implementation_decl_rlist ANTI_ESC
      { rcons (AntiEsc (getANTI_ESC $2) (srclocOf $2)) $1 }
  | objc_implementation_decl_rlist ANTI_EDECL
      { rcons (AntiEdecls (getANTI_EDECL $2) (srclocOf $2)) $1 }
  | objc_implementation_decl_rlist ANTI_EDECLS
      { rcons (AntiEdecls (getANTI_EDECLS $2) (srclocOf $2)) $1 }

property_synthesize :: { Definition }
property_synthesize :
  '@' 'synthesize' property_ivar_rlist semi
    { ObjCSynDef (rev $3) ($1 `srcspan` $4) }

property_ivar_rlist :: { RevList (Id, Maybe Id) }
property_ivar_rlist :
    identifier
      { rsingleton ($1, Nothing) }
  | identifier '=' identifier
      { rsingleton ($1, Just $3) }
  | property_ivar_rlist identifier
      { rcons ($2, Nothing) $1 }
  | property_ivar_rlist identifier '=' identifier
      { rcons ($2, Just $4) $1 }

property_dynamic :: { Definition }
property_dynamic :
  '@' 'dynamic' identifier_rlist semi
    { ObjCDynDef (rev $3) ($1 `srcspan` $4) }

objc_method_definition :: { Definition }
objc_method_definition :
    objc_method_proto semi compound_statement
      { let stmts = mkBlockItems $3
        in
          ObjCMethDef $1 stmts ($1 `srcspan` $3)
      }
  | objc_method_proto      compound_statement
      { let stmts = mkBlockItems $2
        in
          ObjCMethDef $1 stmts ($1 `srcspan` $2)
      }
  | ANTI_OBJC_METHOD_DEF
      { AntiObjCMeth (getANTI_OBJC_METHOD_DEF $1) (srclocOf $1) }
  | ANTI_OBJC_METHOD_DEFS
      { AntiObjCMeths (getANTI_OBJC_METHOD_DEFS $1) (srclocOf $1) }

-- Objective-C extension: compatibility alias
--
-- objc-compatibility-alias ->
--   '@' 'compatibility_alias' identifier class-name ';'
--
objc_compatibility_alias :: { Definition }
objc_compatibility_alias :
  '@' 'compatibility_alias' identifier OBJCNAMED semi
      {% do { addClassdefId $3
            ; return $ ObjCCompAlias $3 (Id (getOBJCNAMED $1) (srclocOf $1)) ($1 `srcspan` $5)
            }
      }

{------------------------------------------------------------------------------
 -
 - CUDA
 -
 ------------------------------------------------------------------------------}
cuda_lambda_expression :: { Exp }
cuda_lambda_expression : cuda_lambda_introducer cuda_lambda_declarator compound_statement
    {% do { assertCudaEnabled ($1 <--> $3) "To use lambda-expressions, enable support for CUDA"
          ; let items = mkBlockItems $3
          ; return $ Lambda $1 $2 items ($1 `srcspan` $3)
          }
    }

cuda_lambda_declarator :: { Maybe LambdaDeclarator }
cuda_lambda_declarator :
      cuda_lambda_param_list cuda_lambda_mutable cuda_lambda_return_type { Just $ LambdaDeclarator $1 $2 $3 ($1 `srcspan` $3) }
    | {- empty -} { Nothing }

cuda_lambda_param_list :: { Params }
cuda_lambda_param_list :
      '(' ')' { Params [] False ($1 `srcspan` $2) }
    | '(' parameter_type_list ')' { $2 }

cuda_lambda_mutable :: { Bool }
cuda_lambda_mutable :
      'mutable'   { True  }
    | {- Empty -} { False }

cuda_lambda_return_type :: { Maybe Type }
cuda_lambda_return_type :
    {- Empty -} { Nothing }
    -- FIXME: There should be possibility to explicitly state returned type.
    -- | '->' type_name { Just ($2::Type) }

cuda_lambda_introducer :: { LambdaIntroducer }
cuda_lambda_introducer :
    '[' cuda_lambda_capture_items ']' { LambdaIntroducer $2 ($1 `srcspan` $3)}

cuda_lambda_capture_items :: { [CaptureListEntry] }
cuda_lambda_capture_items :
      '&' { [DefaultByReference] }
    | '=' { [DefaultByValue] }
    | {- empty -}  { [] }


execution_configuration :: { ExeConfig }
execution_configuration :
  argument_expression_list
    {%do {  let args = $1
         ;  when (length args < 2 || length args > 4) $ do
                parserError (locOf args) $
                  text "execution context should have 2-4 arguments, but saw" <+>
                  ppr (length args)
         ;  return $
            case args of
              { [gridDim, blockDim] ->
                    ExeConfig  gridDim blockDim
                               Nothing Nothing
                             (srclocOf args)
              ; [gridDim, blockDim, sharedSize] ->
                    ExeConfig  gridDim blockDim
                               (Just sharedSize) Nothing
                               (srclocOf args)
              ; [gridDim, blockDim, sharedSize, exeStream] ->
                    ExeConfig  gridDim blockDim
                               (Just sharedSize) (Just exeStream)
                               (srclocOf args)
              }
         }
    }

{
happyError :: L T.Token -> P a
happyError (L loc t) =
    parserError (locStart loc) (text "parse error on" <+> quoteTok (ppr t))

getCHAR        (L _ (T.TcharConst x))        = x
getSTRING      (L _ (T.TstringConst x))      = x
getINT         (L _ (T.TintConst x))         = x
getLONG        (L _ (T.TlongIntConst x))     = x
getLONG_LONG   (L _ (T.TlongLongIntConst x)) = x
getFLOAT       (L _ (T.TfloatConst x))       = x
getDOUBLE      (L _ (T.TdoubleConst x))      = x
getLONG_DOUBLE (L _ (T.TlongDoubleConst x))  = x
getID          (L _ (T.Tidentifier ident))   = ident
getNAMED       (L _ (T.Tnamed ident))        = ident
getOBJCNAMED   (L _ (T.TObjCnamed ident))    = ident

getPRAGMA      (L _ (T.Tpragma pragma))      = pragma

getCOMMENT     (L _ (T.Tcomment comment))    = comment

getANTI_ID          (L _ (T.Tanti_id v))          = v
getANTI_CONST       (L _ (T.Tanti_const v))       = v
getANTI_INT         (L _ (T.Tanti_int v))         = v
getANTI_UINT        (L _ (T.Tanti_uint v))        = v
getANTI_LINT        (L _ (T.Tanti_lint v))        = v
getANTI_ULINT       (L _ (T.Tanti_ulint v))       = v
getANTI_LLINT       (L _ (T.Tanti_llint v))       = v
getANTI_ULLINT      (L _ (T.Tanti_ullint v))      = v
getANTI_FLOAT       (L _ (T.Tanti_float v))       = v
getANTI_DOUBLE      (L _ (T.Tanti_double v))      = v
getANTI_LONG_DOUBLE (L _ (T.Tanti_long_double v)) = v
getANTI_CHAR        (L _ (T.Tanti_char v))        = v
getANTI_STRING      (L _ (T.Tanti_string v))      = v
getANTI_EXP         (L _ (T.Tanti_exp v))         = v
getANTI_FUNC        (L _ (T.Tanti_func v))        = v
getANTI_ARGS        (L _ (T.Tanti_args v))        = v
getANTI_DECL        (L _ (T.Tanti_decl v))        = v
getANTI_DECLS       (L _ (T.Tanti_decls v))       = v
getANTI_SDECL       (L _ (T.Tanti_sdecl v))       = v
getANTI_SDECLS      (L _ (T.Tanti_sdecls v))      = v
getANTI_ENUM        (L _ (T.Tanti_enum v))        = v
getANTI_ENUMS       (L _ (T.Tanti_enums v))       = v
getANTI_ESC         (L _ (T.Tanti_esc v))         = v
getANTI_ESCSTM      (L _ (T.Tanti_escstm v))      = v
getANTI_EDECL       (L _ (T.Tanti_edecl v))       = v
getANTI_EDECLS      (L _ (T.Tanti_edecls v))      = v
getANTI_ITEM        (L _ (T.Tanti_item v))        = v
getANTI_ITEMS       (L _ (T.Tanti_items v))       = v
getANTI_STM         (L _ (T.Tanti_stm v))         = v
getANTI_STMS        (L _ (T.Tanti_stms v))        = v
getANTI_TYPE_QUAL   (L _ (T.Tanti_type_qual v))   = v
getANTI_TYPE_QUALS  (L _ (T.Tanti_type_quals v))  = v
getANTI_TYPE        (L _ (T.Tanti_type v))        = v
getANTI_SPEC        (L _ (T.Tanti_spec v))        = v
getANTI_PARAM       (L _ (T.Tanti_param v))       = v
getANTI_PARAMS      (L _ (T.Tanti_params v))      = v
getANTI_PRAGMA      (L _ (T.Tanti_pragma v))      = v
getANTI_COMMENT     (L _ (T.Tanti_comment v))     = v
getANTI_INIT        (L _ (T.Tanti_init v))        = v
getANTI_INITS       (L _ (T.Tanti_inits v))       = v
getANTI_ATTR        (L _ (T.Tanti_attr v))        = v
getANTI_ATTRS       (L _ (T.Tanti_attrs v))       = v

--
-- Objective-C
--
getANTI_OBJC_IFDECL       (L _ (T.Tanti_objc_ifdecl v))       = v
getANTI_OBJC_IFDECLS      (L _ (T.Tanti_objc_ifdecls v))      = v
getANTI_OBJC_PROP         (L _ (T.Tanti_objc_prop v))         = v
getANTI_OBJC_PROPS        (L _ (T.Tanti_objc_props v))        = v
getANTI_OBJC_PROP_ATTR    (L _ (T.Tanti_objc_prop_attr v))    = v
getANTI_OBJC_PROP_ATTRS   (L _ (T.Tanti_objc_prop_attrs v))   = v
getANTI_OBJC_DICTS        (L _ (T.Tanti_objc_dicts v))        = v
getANTI_OBJC_PARAM        (L _ (T.Tanti_objc_param v))        = v
getANTI_OBJC_PARAMS       (L _ (T.Tanti_objc_params v))       = v
getANTI_OBJC_METHOD_PROTO (L _ (T.Tanti_objc_method_proto v)) = v
getANTI_OBJC_METHOD_DEF   (L _ (T.Tanti_objc_method_def v))   = v
getANTI_OBJC_METHOD_DEFS  (L _ (T.Tanti_objc_method_defs v))  = v
getANTI_OBJC_RECV         (L _ (T.Tanti_objc_recv v))         = v
getANTI_OBJC_ARG          (L _ (T.Tanti_objc_arg v))          = v
getANTI_OBJC_ARGS         (L _ (T.Tanti_objc_args v))         = v

lexer :: (L T.Token -> P a) -> P a
lexer cont = do
    t <- lexToken
    setCurToken t
    cont t

locate :: Loc -> (SrcLoc -> a) -> L a
locate loc f = L loc (f (SrcLoc loc))

data TySpec = TSauto !SrcLoc
            | TSregister !SrcLoc
            | TSstatic !SrcLoc
            | TSextern (Maybe Linkage) !SrcLoc
            | TStypedef !SrcLoc

            | TSconst    !SrcLoc
            | TSvolatile !SrcLoc

            | TSsigned   !SrcLoc
            | TSunsigned !SrcLoc

            | TSvoid   !SrcLoc
            | TSchar   !SrcLoc
            | TSshort  !SrcLoc
            | TSlong   !SrcLoc
            | TSint    !SrcLoc
            | TSfloat  !SrcLoc
            | TSdouble !SrcLoc

            | TSstruct (Maybe Id) (Maybe [FieldGroup]) [Attr] !SrcLoc
            | TSunion (Maybe Id) (Maybe [FieldGroup]) [Attr] !SrcLoc
            | TSenum (Maybe Id) [CEnum] [Attr] !SrcLoc
            | TSnamed Id [Id] !SrcLoc           -- the '[Id]' are Objective-C protocol references

            | TSAntiTypeQual String !SrcLoc
            | TSAntiTypeQuals String !SrcLoc

            -- C99
            | TS_Bool      !SrcLoc
            | TS_Complex   !SrcLoc
            | TS_Imaginary !SrcLoc
            | TSinline     !SrcLoc
            | TSrestrict   !SrcLoc

            -- GCC
            | TStypeofExp Exp !SrcLoc
            | TStypeofType Type !SrcLoc
            | TSva_list !SrcLoc
            | TSAttr Attr
            | TS__restrict !SrcLoc

            -- Clang blocks
            | TS__block !SrcLoc

            -- Objective-C
            | TSObjC__weak !SrcLoc
            | TSObjC__strong !SrcLoc
            | TSObjC__unsafe_unretained !SrcLoc

            -- CUDA
            | TSCUDAdevice !SrcLoc
            | TSCUDAglobal !SrcLoc
            | TSCUDAhost !SrcLoc
            | TSCUDAconstant !SrcLoc
            | TSCUDAshared !SrcLoc
            | TSCUDArestrict !SrcLoc
            | TSCUDAnoinline !SrcLoc

            -- OpenCL
            | TSCLprivate !SrcLoc
            | TSCLlocal !SrcLoc
            | TSCLglobal !SrcLoc
            | TSCLconstant !SrcLoc
            | TSCLreadonly !SrcLoc
            | TSCLwriteonly !SrcLoc
            | TSCLkernel !SrcLoc
  deriving (Eq, Ord, Show)

instance Located TySpec where
    locOf (TSauto loc)            = locOf loc
    locOf (TSregister loc)        = locOf loc
    locOf (TSstatic loc)          = locOf loc
    locOf (TSextern _ loc)        = locOf loc
    locOf (TStypedef loc)         = locOf loc

    locOf (TSconst loc)           = locOf loc
    locOf (TSvolatile loc)        = locOf loc

    locOf (TSsigned loc)          = locOf loc
    locOf (TSunsigned loc)        = locOf loc

    locOf (TSvoid loc)            = locOf loc
    locOf (TSchar loc)            = locOf loc
    locOf (TSshort loc)           = locOf loc
    locOf (TSint loc)             = locOf loc
    locOf (TSlong loc)            = locOf loc
    locOf (TSfloat loc)           = locOf loc
    locOf (TSdouble loc)          = locOf loc

    locOf (TSstruct _ _ _ loc)    = locOf loc
    locOf (TSunion _ _ _ loc)     = locOf loc
    locOf (TSenum _ _ _ loc)      = locOf loc
    locOf (TSnamed _ _ loc)       = locOf loc

    locOf (TSAntiTypeQual _ loc)  = locOf loc
    locOf (TSAntiTypeQuals _ loc) = locOf loc

    locOf (TS_Bool loc)           = locOf loc
    locOf (TS_Complex loc)        = locOf loc
    locOf (TS_Imaginary loc)      = locOf loc
    locOf (TSinline loc)          = locOf loc
    locOf (TSrestrict loc)        = locOf loc

    locOf (TStypeofExp _ loc)     = locOf loc
    locOf (TStypeofType _ loc)    = locOf loc
    locOf (TSva_list loc)         = locOf loc
    locOf (TSAttr attr)           = locOf attr
    locOf (TS__restrict loc)      = locOf loc

    locOf (TS__block loc)         = locOf loc

    locOf (TSObjC__weak loc)      = locOf loc
    locOf (TSObjC__strong loc)    = locOf loc
    locOf (TSObjC__unsafe_unretained loc)
                                  = locOf loc

    locOf (TSCUDAdevice loc)      = locOf loc
    locOf (TSCUDAglobal loc)      = locOf loc
    locOf (TSCUDAhost loc)        = locOf loc
    locOf (TSCUDAconstant loc)    = locOf loc
    locOf (TSCUDAshared loc)      = locOf loc
    locOf (TSCUDArestrict loc)    = locOf loc
    locOf (TSCUDAnoinline loc)    = locOf loc

    locOf (TSCLprivate loc)       = locOf loc
    locOf (TSCLlocal loc)         = locOf loc
    locOf (TSCLglobal loc)        = locOf loc
    locOf (TSCLconstant loc)      = locOf loc
    locOf (TSCLreadonly loc)      = locOf loc
    locOf (TSCLwriteonly loc)     = locOf loc
    locOf (TSCLkernel loc)        = locOf loc

instance Pretty TySpec where
    ppr (TSauto _)                    = text "auto"
    ppr (TSregister _)                = text "register"
    ppr (TSstatic _)                  = text "static"
    ppr (TSextern Nothing _)          = text "extern"
    ppr (TSextern (Just l) _)         = text "extern" <+> ppr l
    ppr (TStypedef _)                 = text "typedef"

    ppr (TSconst _)    = text "const"
    ppr (TSvolatile _) = text "volatile"

    ppr (TSsigned _)   = text "signed"
    ppr (TSunsigned _) = text "unsigned"

    ppr (TSvoid _)     = text "void"
    ppr (TSchar _)     = text "char"
    ppr (TSshort _)    = text "short"
    ppr (TSint _)      = text "int"
    ppr (TSlong _)     = text "long"
    ppr (TSfloat _)    = text "float"
    ppr (TSdouble _)   = text "double"

    ppr (TSstruct maybe_id maybe_fields attrs _) =
        pprStructOrUnion "struct" maybe_id maybe_fields attrs

    ppr (TSunion maybe_id maybe_fields attrs _) =
        pprStructOrUnion "union" maybe_id maybe_fields attrs

    ppr (TSenum maybe_id cenums attrs _) =
        pprEnum maybe_id cenums attrs

    ppr (TSnamed ident ps _) =
        ppr ident <> if null ps then empty else angles (commasep (map ppr ps))

    ppr (TS_Bool _)      = text "_Bool"
    ppr (TS_Complex _)   = text "_Complex"
    ppr (TS_Imaginary _) = text "_Imaginary"
    ppr (TSinline _)     = text "inline"
    ppr (TSrestrict _)   = text "restrict"

    ppr (TStypeofExp e _)   = text "__typeof__" <> parens (ppr e)
    ppr (TStypeofType ty _) = text "__typeof__" <> parens (ppr ty)
    ppr (TSva_list _)       = text "__builtin_va_list"
    ppr (TSAttr attr)       = ppr [attr]
    ppr (TS__restrict _)    = text "__restrict"

    ppr (TS__block _) = text "__block"

    ppr (TSObjC__weak _)              = text "__weak"
    ppr (TSObjC__strong _)            = text "__strong"
    ppr (TSObjC__unsafe_unretained _) = text "__unsafe_unretained"

    ppr (TSCUDAdevice _)    = text "__device__"
    ppr (TSCUDAglobal _)    = text "__global__"
    ppr (TSCUDAhost _)      = text "__host__"
    ppr (TSCUDAconstant _)  = text "__constant__"
    ppr (TSCUDAshared _)    = text "__shared__"
    ppr (TSCUDArestrict _)  = text "__restrict__"
    ppr (TSCUDAnoinline _)  = text "__noinline__"

    ppr (TSCLprivate _)     = text "__private"
    ppr (TSCLlocal _)       = text "__local"
    ppr (TSCLglobal _)      = text "__global"
    ppr (TSCLconstant _)    = text "__constant"
    ppr (TSCLreadonly _)    = text "read_only"
    ppr (TSCLwriteonly _)   = text "write_only"
    ppr (TSCLkernel _)      = text "__kernel"

isStorage :: TySpec -> Bool
isStorage (TSauto _)                    = True
isStorage (TSregister _)                = True
isStorage (TSstatic _)                  = True
isStorage (TSextern _ _)                = True
isStorage (TStypedef _)                 = True
isStorage (TS__block _)                 = True
isStorage (TSObjC__weak _)              = True
isStorage (TSObjC__strong _)            = True
isStorage (TSObjC__unsafe_unretained _) = True
isStorage _                             = False

mkStorage :: [TySpec] -> [Storage]
mkStorage specs = map mk (filter isStorage specs)
    where
      mk :: TySpec -> Storage
      mk (TSauto loc)                    = Tauto loc
      mk (TSregister loc)                = Tregister loc
      mk (TSstatic loc)                  = Tstatic loc
      mk (TSextern l loc)                = Textern l loc
      mk (TStypedef loc)                 = Ttypedef loc
      mk (TS__block loc)                 = T__block loc
      mk (TSObjC__weak loc)              = TObjC__weak loc
      mk (TSObjC__strong loc)            = TObjC__strong loc
      mk (TSObjC__unsafe_unretained loc) = TObjC__unsafe_unretained loc
      mk _                               = error "internal error in mkStorage"

isTypeQual :: TySpec -> Bool
isTypeQual (TSconst _)          = True
isTypeQual (TSvolatile _)       = True
isTypeQual (TSAntiTypeQual {})  = True
isTypeQual (TSAntiTypeQuals {}) = True
isTypeQual (TSinline _)         = True
isTypeQual (TSrestrict _)       = True
isTypeQual (TSAttr _)           = True
isTypeQual (TS__restrict _)     = True
isTypeQual (TSCUDAdevice _)     = True
isTypeQual (TSCUDAglobal _)     = True
isTypeQual (TSCUDAhost _)       = True
isTypeQual (TSCUDAconstant _)   = True
isTypeQual (TSCUDAshared _)     = True
isTypeQual (TSCUDArestrict _)   = True
isTypeQual (TSCUDAnoinline _)   = True
isTypeQual (TSCLprivate _)      = True
isTypeQual (TSCLlocal _)        = True
isTypeQual (TSCLglobal _)       = True
isTypeQual (TSCLconstant _)     = True
isTypeQual (TSCLreadonly _)     = True
isTypeQual (TSCLwriteonly _)    = True
isTypeQual (TSCLkernel _)       = True
isTypeQual _                    = False

mkTypeQuals :: [TySpec] -> [TypeQual]
mkTypeQuals specs = map mk (filter isTypeQual specs)
    where
      mk :: TySpec -> TypeQual
      mk (TSconst loc)           = Tconst loc
      mk (TSvolatile loc)        = Tvolatile loc
      mk (TSAntiTypeQual v loc)  = AntiTypeQual v loc
      mk (TSAntiTypeQuals v loc) = AntiTypeQuals v loc
      mk (TSinline loc)          = Tinline loc
      mk (TSrestrict loc)        = Trestrict loc
      mk (TSAttr attr)           = TAttr attr
      mk (TS__restrict loc)      = T__restrict loc
      mk (TSCUDAdevice loc)      = TCUDAdevice loc
      mk (TSCUDAglobal loc)      = TCUDAglobal loc
      mk (TSCUDAhost loc)        = TCUDAhost loc
      mk (TSCUDAconstant loc)    = TCUDAconstant loc
      mk (TSCUDAshared loc)      = TCUDAshared loc
      mk (TSCUDArestrict loc)    = TCUDArestrict loc
      mk (TSCUDAnoinline loc)    = TCUDAnoinline loc
      mk (TSCLprivate loc)       = TCLprivate loc
      mk (TSCLlocal loc)         = TCLlocal loc
      mk (TSCLglobal loc)        = TCLglobal loc
      mk (TSCLconstant loc)      = TCLconstant loc
      mk (TSCLreadonly loc)      = TCLreadonly loc
      mk (TSCLwriteonly loc)     = TCLwriteonly loc
      mk (TSCLkernel loc)        = TCLkernel loc
      mk _                       = error "internal error in mkTypeQual"

isSign :: TySpec -> Bool
isSign (TSsigned _)    = True
isSign (TSunsigned _)  = True
isSign _               = False

hasSign :: [TySpec] -> Bool
hasSign specs = any isSign specs

mkSign :: [TySpec] -> P (Maybe Sign)
mkSign specs =
    case filter isSign specs of
      []               -> return Nothing
      [TSunsigned loc] -> return (Just (Tunsigned loc))
      [TSsigned loc]   -> return (Just (Tsigned loc))
      [_]              -> fail "internal error in mkSign"
      _                -> fail "multiple signs specified"

checkNoSign :: [TySpec] -> String -> P ()
checkNoSign spec msg  | hasSign spec  = fail msg
                      | otherwise     = return ()

isAttr :: TySpec -> Bool
isAttr (TSAttr _)  = True
isAttr _           = False

checkOnlyAttributes :: [TySpec] -> P [Attr]
checkOnlyAttributes specs =
    case filter (not . isAttr) specs of
       [] -> return attrs
       spec : _ -> expected ["attribute"] (Just (show spec))
  where
    attrs :: [Attr]
    attrs = [attr | TSAttr attr <- specs]

mkStringConst :: StringLit -> Const
mkStringConst (StringLit raw s l) =
    StringConst raw s l

composeDecls :: Decl -> Decl -> Decl
composeDecls (DeclRoot _) root =
    root

composeDecls (C.Ptr quals decl loc) root =
    C.Ptr quals (composeDecls decl root) loc

composeDecls (C.BlockPtr quals decl loc) root =
    C.BlockPtr quals (composeDecls decl root) loc

composeDecls (Array quals size decl loc) root =
    Array quals size (composeDecls decl root) loc

composeDecls (Proto decl args loc) root =
    Proto (composeDecls decl root) args loc

composeDecls (OldProto decl args loc) root =
    OldProto (composeDecls decl root) args loc

mkDeclSpec :: [TySpec] -> P DeclSpec
mkDeclSpec specs =
    go (sort rest)
  where
    storage ::[Storage]
    storage = mkStorage specs

    quals :: [TypeQual]
    quals = mkTypeQuals specs

    -- All TypeQuals except for attributes
    qualsNoAttrs :: [TypeQual]
    qualsNoAttrs = mkTypeQuals (filter (not . isAttr) specs)

    -- Attributes pulled from the TySpecs
    attrTySpecs :: [Attr]
    attrTySpecs = [attr | TSAttr attr <- specs]

    rest :: [TySpec]
    rest = [x  |  x <- specs
               ,  not (isStorage x) && not (isTypeQual x) && not (isSign x)]

    go :: [TySpec] -> P DeclSpec
    go [TSvoid l] = do
        checkNoSign specs "sign specified for void type"
        return $ cdeclSpec storage quals (Tvoid l)

    go [TSchar l] = do
        sign <- mkSign specs
        return $ cdeclSpec storage quals (Tchar sign l)

    go [TSshort l] = do
        sign <- mkSign specs
        return $ cdeclSpec storage quals (Tshort sign l)

    go [TSshort _, TSint _] = do
        sign <- mkSign specs
        return $ cdeclSpec storage quals (Tshort sign (srclocOf rest))

    go [TSint l] = do
        sign <- mkSign specs
        return $ cdeclSpec storage quals (Tint sign l)

    go [TSlong l] = do
        sign <- mkSign specs
        return $ cdeclSpec storage quals (Tlong sign l)

    go [TSlong _, TSint _] = do
        sign <- mkSign specs
        return $ cdeclSpec storage quals (Tlong sign (srclocOf rest))

    go [TSlong _, TSlong _] = do
        sign <- mkSign specs
        return $ cdeclSpec storage quals (Tlong_long sign (srclocOf rest))

    go [TSlong _, TSlong _, TSint _] = do
        sign <- mkSign specs
        return $ cdeclSpec storage quals (Tlong_long sign (srclocOf rest))

    go [TSfloat loc] = do
        checkNoSign specs "sign specified for float type"
        return $ cdeclSpec storage quals (Tfloat loc)

    go [TSdouble loc] = do
        checkNoSign specs "sign specified for double type"
        return $ cdeclSpec storage quals (Tdouble loc)

    go [TSlong _, TSdouble _] = do
        checkNoSign specs "sign specified for long double type"
        return $ cdeclSpec storage quals (Tlong_double (srclocOf rest))

    go [TSfloat _, TS_Complex _] = do
        checkNoSign specs "sign specified for float _Complex type"
        return $ cdeclSpec storage quals (Tfloat_Complex (srclocOf rest))

    go [TSdouble _, TS_Complex _] = do
        checkNoSign specs "sign specified for double _Complex type"
        return $ cdeclSpec storage quals (Tdouble_Complex (srclocOf rest))

    go [TSlong _, TSdouble _, TS_Complex _] = do
        checkNoSign specs "sign specified for long double _Complex type"
        return $ cdeclSpec storage quals (Tlong_double_Complex (srclocOf rest))

    go [TSfloat _, TS_Imaginary _] = do
        checkNoSign specs "sign specified for float _Imaginary type"
        return $ cdeclSpec storage quals (Tfloat_Imaginary (srclocOf rest))

    go [TSdouble _, TS_Imaginary _] = do
        checkNoSign specs "sign specified for double _Imaginary type"
        return $ cdeclSpec storage quals (Tdouble_Imaginary (srclocOf rest))

    go [TSlong _, TSdouble _, TS_Imaginary _] = do
        checkNoSign specs "sign specified for long double _Imaginary type"
        return $ cdeclSpec storage quals (Tlong_double_Imaginary (srclocOf rest))

    -- Attributes for structs, unions, and enums may appear after the closing
    -- brace. If this happens, they end up in the list of TypeQuals. We pull
    -- them out here and associate them with the struct/union/enum.
    go [TSstruct ident fields attrs loc] = do
        checkNoSign specs "sign specified for struct type"
        return $ cdeclSpec storage qualsNoAttrs
            (Tstruct ident fields (attrTySpecs ++ attrs) loc)

    go [TSunion ident fields attrs loc] = do
        checkNoSign specs "sign specified for union type"
        return $ cdeclSpec storage qualsNoAttrs
            (Tunion ident fields (attrTySpecs ++ attrs) loc)

    go [TSenum ident enums attrs loc] = do
        checkNoSign specs "sign specified for enum type"
        return $ cdeclSpec storage qualsNoAttrs
            (Tenum ident enums (attrTySpecs ++ attrs) loc)

    go [TSnamed ident refs loc] = do
        checkNoSign specs "sign specified for named type"
        return $ cdeclSpec storage quals (Tnamed ident refs loc)

    go [TStypeofExp e loc] = do
        checkNoSign specs "sign specified for typeof"
        return $ cdeclSpec storage quals (TtypeofExp e loc)

    go [TStypeofType ty loc] = do
        checkNoSign specs "sign specified for typeof"
        return $ cdeclSpec storage quals (TtypeofType ty loc)

    go [TS_Bool l] = do
        checkNoSign specs "sign specified for _Bool"
        return $ cdeclSpec storage quals (T_Bool l)

    go [TSva_list l] = do
        checkNoSign specs "sign specified for __builtin_va_list"
        return $ cdeclSpec storage quals (Tva_list l)

    go [] = do
        sign <- mkSign specs
        return $ cdeclSpec storage quals (Tint sign (storage `srcspan` quals))

    go tyspecs =
        throw $ ParserException (locOf tyspecs)
            (text "bad type:" <+> spread (map ppr tyspecs))

mkPtr :: [TySpec] -> Decl -> Decl
mkPtr specs decl = C.Ptr quals decl (specs `srcspan` decl)
  where
    quals = mkTypeQuals specs

mkBlockPtr :: Loc -> [TySpec] -> P (Decl -> Decl)
mkBlockPtr loc specs = do
    assertBlocksEnabled loc "To use blocks, enable the blocks language extension"
    return $ \decl -> C.BlockPtr quals decl (specs `srcspan` decl)
  where
    quals = mkTypeQuals specs

mkArray :: [TySpec] -> ArraySize -> Decl -> Decl
mkArray specs size decl = Array quals size decl (specs `srcspan` decl)
  where
    quals = mkTypeQuals specs

mkProto :: Params -> Decl -> Decl
mkProto args decl = Proto decl args (args `srcspan` decl)

mkOldProto :: [Id] -> Decl -> Decl
mkOldProto args decl = OldProto decl args (args `srcspan` decl)

checkInitGroup :: DeclSpec -> Decl -> [Attr] -> [Init] -> P InitGroup
checkInitGroup dspec decl attrs inits =
    go dspec
  where
    go :: DeclSpec -> P InitGroup
    go (DeclSpec storage quals tspec _) | any isTypedef storage = do
        typedefs    <-  mapM checkInit inits'
        let dspec'  =   cdeclSpec storage' quals tspec
        return $ ctypedefGroup dspec' attrs typedefs
      where
        storage' :: [Storage]
        storage' = [x | x <- storage, (not . isTypedef) x]

        isTypedef :: Storage -> Bool
        isTypedef (Ttypedef _)  = True
        isTypedef _             = False

        checkInit :: Init -> P Typedef
        checkInit init@(Init ident _  _ (Just _) _ _)=
            throw $ ParserException (locOf init) $
                text "typedef" <+>
                quoteTok (ppr ident) <+>
                text "is illegaly initialized"

        checkInit (Init ident@(Id name _) decl _ _ attrs _) = do
            addTypedef name
            return $ ctypedef ident decl attrs

        checkInit (Init ident@(AntiId _ _) decl _ _ attrs _) =
            return $ ctypedef ident decl attrs

    go _ = do
        mapM_ checkInit inits'
        return $ cinitGroup dspec attrs inits'
      where
        checkInit :: Init -> P ()
        checkInit (Init (Id name _) _ _ _ _ _)   = addVariable name
        checkInit (Init (AntiId _ _) _ _ _ _ _)  = return ()

    composeInitDecl :: Decl -> Init -> Init
    composeInitDecl decl (Init ident initDecl maybe_asmlabel maybe_exp attrs loc) =
        Init ident (composeDecls initDecl decl) maybe_asmlabel maybe_exp attrs loc

    inits' = map (composeInitDecl decl) inits

    loc :: Loc
    loc = dspec <--> attrs <--> inits

checkAnonymousStructOrUnion :: L T.Token -> DeclSpec -> P ()
checkAnonymousStructOrUnion _   (DeclSpec _ _ (Tstruct {}) _) = return ()
checkAnonymousStructOrUnion _   (DeclSpec _ _ (Tunion {}) _)  = return ()
checkAnonymousStructOrUnion tok (DeclSpec _ _ (Tunion {}) _)  =
    expectedAt tok ["anonymous struct or union"] Nothing

declRoot :: Located a => a -> Decl
declRoot x = DeclRoot (srclocOf x)

addClassdefId :: Id -> P ()
addClassdefId (Id str _)  = addClassdef str
addClassdefId (AntiId {}) = return ()

assertBlocksEnabled :: Loc -> String -> P ()
assertBlocksEnabled loc errMsg = do
    blocks_enabled <- useBlocksExts
    unless blocks_enabled $
     throw $ ParserException loc $ text errMsg

expectedObjCPropertyAttr :: Loc -> P a
expectedObjCPropertyAttr loc =
    throw $ ParserException loc $
      text "Expected an Objective-C property attribute; allowed are the following:" </>
      nest 2
        (text "'getter = <sel>', 'setter = <sel>:', 'readonly', 'readwrite', 'assign'," <+>
         text "'retain', 'copy', 'nonatomic', 'atomic', 'strong', 'weak', and 'unsafe_unretained'")

assertObjCEnabled :: Loc -> String -> P ()
assertObjCEnabled loc errMsg = do
    objc_enabled <- useObjCExts
    unless objc_enabled $
     throw $ ParserException loc $ text errMsg

assertCudaEnabled :: Loc -> String -> P ()
assertCudaEnabled loc errMsg = do
 cuda_enabled <- useCUDAExts
 unless cuda_enabled $
  throw $ ParserException loc $ text errMsg

gccOnly :: Located a => String -> a -> P a
gccOnly errMsg x = do
  gcc_enabled <- useGccExts
  unless gcc_enabled $
    throw $ ParserException (locOf x) $ text errMsg
  pure x

mkBlock :: [BlockItem] -> SrcLoc -> Stm
mkBlock items@[BlockStm AntiStms{}] sloc = Block items sloc
mkBlock [BlockStm stm]              _    = stm
mkBlock items                       sloc = Block items sloc

mkBlockItems :: Stm -> [BlockItem]
mkBlockItems (Block items _) = items
mkBlockItems stm             = [BlockStm stm]

mkCommentStm :: L T.Token -> Stm -> Stm
mkCommentStm tok stm = Comment (getCOMMENT tok) stm (srclocOf tok)

mkEmptyCommentStm :: L T.Token -> Stm
mkEmptyCommentStm tok = mkCommentStm tok (Exp Nothing noLoc)

data RevList a  =  RNil
                |  RCons a (RevList a)
                |  RApp [a] (RevList a)

rnil :: RevList a
rnil = RNil

rsingleton :: a -> RevList a
rsingleton x = RCons x RNil

infixr 5 `rcons`

rcons :: a -> RevList a -> RevList a
rcons x xs  = RCons x xs

rapp :: [a] -> RevList a -> RevList a
rapp xs ys  = RApp xs ys

rlist :: [a] -> RevList a
rlist xs = rlist' xs rnil
  where
    rlist' []     acc = acc
    rlist' (x:xs) acc = rlist' xs (rcons x acc)

rev :: RevList a -> [a]
rev xs = go [] xs
  where
    go  l  RNil          = l
    go  l  (RCons x xs)  = go (x : l) xs
    go  l  (RApp xs ys)  = go (xs ++ l) ys

instance Located a => Located (RevList a) where
    locOf RNil         = mempty
    locOf (RCons x xs) = locOf x `mappend` locOf xs
    locOf (RApp xs ys) = locOf xs `mappend` locOf ys
}
