-- -*- mode: literate-haskell -*-

{
{-# OPTIONS -w #-}

-- |
-- Module      :  Language.C.Parser.Parser
-- Copyright   :  (c) Harvard University 2006-2011
--                (c) Geoffrey Mainland 2011-2012
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu

module Language.C.Parser.Parser where

import Control.Monad (forM_,
                      when)
import Control.Monad.Exception
import Data.List (intersperse)
import Data.Loc
import Data.Maybe (catMaybes)
import Text.PrettyPrint.Mainland

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
 'inline'     { L _ T.Tinline }
 'int'        { L _ T.Tint }
 'long'       { L _ T.Tlong }
 'register'   { L _ T.Tregister }
 'restrict'   { L _ T.Trestrict }
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
 '_Bool'      { L _ T.TBool }
 '_Complex'   { L _ T.TComplex }
 '_Imaginary' { L _ T.TImaginary }

 '#pragma'    { L _ (T.Tpragma _) }

 '__asm__'           { L _ T.Tasm }
 '__attribute__'     { L _ T.Tattribute }
 '__extension__'     { L _ T.Textension }
 '__builtin_va_arg'  { L _ T.Tbuiltin_va_arg }
 '__builtin_va_list' { L _ T.Tbuiltin_va_list }
 '__typeof__'        { L _ T.Ttypeof }

 '<<<'           { L _ T.TCUDA3lt }
 '>>>'           { L _ T.TCUDA3gt }
 '__device__'    { L _ T.TCUDAdevice }
 '__global__'    { L _ T.TCUDAglobal }
 '__host__'      { L _ T.TCUDAhost }
 '__constant__'  { L _ T.TCUDAconstant }
 '__shared__'    { L _ T.TCUDAshared }
 '__restrict__'  { L _ T.TCUDArestrict }
 '__noinline__'  { L _ T.TCUDAnoinline }

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

 'typename'       { L _ T.Ttypename }

 ANTI_ID          { L _ (T.Tanti_id _) }
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
 ANTI_EDECL       { L _ (T.Tanti_edecl _) }
 ANTI_EDECLS      { L _ (T.Tanti_edecls _) }
 ANTI_ITEM        { L _ (T.Tanti_item _) }
 ANTI_ITEMS       { L _ (T.Tanti_items _) }
 ANTI_STM         { L _ (T.Tanti_stm _) }
 ANTI_STMS        { L _ (T.Tanti_stms _) }
 ANTI_SPEC        { L _ (T.Tanti_spec _) }
 ANTI_TYPE        { L _ (T.Tanti_type _) }
 ANTI_PARAM       { L _ (T.Tanti_param _) }
 ANTI_PARAMS      { L _ (T.Tanti_params _) }
 ANTI_PRAGMA      { L _ (T.Tanti_pragma _) }

%expect 1

%monad { P } { >>= } { return }
%lexer { lexer } { L _ T.Teof }
%tokentype { (L T.Token) }
%error { happyError }

%name parseExp        expression

%name parseEdecl      external_declaration
%name parseDecl       declaration
%name parseStructDecl struct_declaration
%name parseEnum       enumerator

%name parseType       type_declaration
%name parseParam      parameter_declaration
%name parseInit       initializer

%name parseStm        statement

%name parseUnit       translation_unit
%name parseFunc       function_definition

%right NAMED
%%

{------------------------------------------------------------------------------
 -
 - Identifiers
 -
 ------------------------------------------------------------------------------}

identifier :: { Id }
identifier :
    ID       { Id (getID $1) (srclocOf $1) }
  | ANTI_ID  { AntiId (getANTI_ID $1) (srclocOf $1) }

identifier_or_typedef :: { Id }
identifier_or_typedef :
    identifier  { $1 }
  | NAMED       { Id (getNAMED $1) (srclocOf $1) }

{------------------------------------------------------------------------------
 -
 - Constants
 -
 ------------------------------------------------------------------------------}

constant :: {  Const }
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
  | ANTI_INT          { AntiInt (getANTI_INT $1) (srclocOf $1) }
  | ANTI_UINT         { AntiUInt (getANTI_UINT $1) (srclocOf $1) }
  | ANTI_LINT         { AntiLInt (getANTI_LINT $1) (srclocOf $1) }
  | ANTI_ULINT        { AntiULInt (getANTI_ULINT $1) (srclocOf $1) }
  | ANTI_LLINT        { AntiLLInt (getANTI_LLINT $1) (srclocOf $1) }
  | ANTI_ULLINT       { AntiULLInt (getANTI_ULLINT $1) (srclocOf $1) }
  | ANTI_FLOAT        { AntiFloat (getANTI_FLOAT $1) (srclocOf $1) }
  | ANTI_DOUBLE       { AntiDouble (getANTI_DOUBLE $1) (srclocOf $1) }
  | ANTI_LONG_DOUBLE  { AntiLongDouble (getANTI_LONG_DOUBLE $1) (srclocOf $1) }
  | ANTI_CHAR         { AntiChar (getANTI_CHAR $1) (srclocOf $1) }
  | ANTI_STRING       { AntiString (getANTI_STRING $1) (srclocOf $1) }


{------------------------------------------------------------------------------
 -
 - Expressions
 -
 ------------------------------------------------------------------------------}

primary_expression :: { Exp }
primary_expression :
    identifier
      { Var $1 (srclocOf $1) }
  | constant
      { Const $1 (srclocOf $1) }
  | string_constant
      { let  {  ss   = rev $1
             ;  l    = srclocOf ss
             ;  raw  = map (fst . unLoc) ss
             ;  s    = (concat . intersperse " " . map (snd . unLoc)) ss
             }
        in
          Const (StringConst raw s l) l
      }
  | '(' expression ')'
      { $2 }
  | '(' expression error
      {% unclosed ($1 <--> $2) "(" }
  | '(' compound_statement ')'
      { let Block items _ = $2
        in
          StmExpr items ($1 `srcspan` $3)
      }
  | ANTI_EXP
      { AntiExp (getANTI_EXP $1) (srclocOf $1) }

string_constant :: { RevList (L (String, String)) }
string_constant :
    STRING                  { rsingleton (L (locOf $1) (getSTRING $1)) }
    {- Extension: GCC -}
  | string_constant STRING  { rcons (L (locOf $2) (getSTRING $2)) $1 }

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
      {% unclosed ($2 <--> rev $3) "(" }
  | postfix_expression '(' argument_expression_list ')'
      { FnCall $1 (rev $3) ($1 `srcspan` $4) }

  | postfix_expression '<<<' execution_configuration error
      {% unclosed ($2 <--> $3) "<<<" }
  | postfix_expression '<<<' execution_configuration '>>>' '(' ')'
      { CudaCall $1 $3 [] ($1 `srcspan` $6) }
  | postfix_expression '<<<' execution_configuration '>>>'
                       '(' argument_expression_list error
      {% unclosed ($5 <--> rev $6) "(" }
  | postfix_expression '<<<' execution_configuration '>>>'
                       '(' argument_expression_list ')'
      { CudaCall $1 $3 (rev $6) ($1 `srcspan` $7) }

  | postfix_expression '.' identifier_or_typedef
      { Member $1 $3 ($1 `srcspan` $3) }
  | postfix_expression '->' identifier_or_typedef
      { PtrMember $1 $3 ($1 `srcspan` $3) }
  | postfix_expression '++'
      { PostInc $1 ($1 `srcspan` $2) }
  | postfix_expression '--'
      { PostDec $1 ($1 `srcspan` $2) }
  | '(' type_name ')' '{' initializer_list '}'
      { CompoundLit ($2 :: Type) (rev $5) ($1 `srcspan` $6) }
  | '(' type_name ')' '{' initializer_list ',' '}'
      { CompoundLit $2 (rev $5) ($1 `srcspan` $7) }
  {- Extension: GCC -}
  | '__builtin_va_arg' '(' assignment_expression ',' type_declaration ')'
      { BuiltinVaArg $3 $5 ($1 `srcspan` $6) }

{- Extension: CUDA -}
execution_configuration :: { ExeConfig }
execution_configuration :
  argument_expression_list
    {%do {  let args = rev $1
         ;  when (length args < 2 || length args > 4) $ do
                parserError (locOf (rev $1)) $
                  text "execution context should have 2-4 arguments, but saw" <+>
                  ppr (length args)
         ;  return $
            case args of
              [gridDim, blockDim] ->
                  ExeConfig  gridDim blockDim
                             Nothing Nothing
                             (srclocOf args)
              [gridDim, blockDim, sharedSize] ->
                  ExeConfig  gridDim blockDim
                             (Just sharedSize) Nothing
                             (srclocOf args)
              [gridDim, blockDim, sharedSize, exeStream] ->
                  ExeConfig  gridDim blockDim
                             (Just sharedSize) (Just exeStream)
                             (srclocOf args)
         }
    }

argument_expression_list :: { RevList Exp }
argument_expression_list :
    assignment_expression
      { rsingleton $1 }
  | ANTI_ARGS
      { rsingleton (AntiArgs (getANTI_ARGS $1) (srclocOf $1)) }
  | argument_expression_list ',' assignment_expression
      { rcons $3 $1}
  | argument_expression_list ',' ANTI_ARGS
      { rcons (AntiArgs (getANTI_ARGS $3) (srclocOf $3)) $1 }

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

declaration_ :: { InitGroup }
declaration_ :
    declaration_specifiers
      {% do{ let (dspec, decl)  = $1
           ; checkInitGroup dspec decl [] []
           }
      }
  | declaration_specifiers attributes
      {% do{ let (dspec, decl)  = $1
           ; checkInitGroup dspec decl $2 []
           }
      }
  | declaration_specifiers init_declarator_list
      {% do{ let (dspec, decl)  = $1
           ; let inits          = rev $2
           ; checkInitGroup dspec decl [] (rev $2)
           }
      }
  | declaration_specifiers attributes init_declarator_list
      {% do{ let (dspec, decl) = $1
           ; checkInitGroup dspec decl $2 (rev $3)
           }
      }
  | declaration_specifiers error
      {% do{ let (_, decl)  = $1
           ; expected ["';'"]
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
  | type_specifier declaration_specifiers_
      {% do{ dspec <- mkDeclSpec ($1 : $2)
           ; return (dspec, DeclRoot ($1 `srcspan` $2))
           }
      }
  | storage_qualifier_specifiers type_specifier
      {% do{ dspec <- mkDeclSpec ($1 ++ [$2])
           ; return $(dspec, DeclRoot ($1 `srcspan` $2))
           }
      }
  | storage_qualifier_specifiers type_specifier declaration_specifiers_
      {% do{ dspec <- mkDeclSpec ($1 ++ $2 : $3)
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

declaration_specifiers_ :: { [TySpec] }
declaration_specifiers_ :
    storage_class_specifier %prec NAMED              { [$1] }
  | storage_class_specifier declaration_specifiers_  { $1 : $2 }
  | type_specifier %prec NAMED                       { [$1] }
  | type_specifier declaration_specifiers_           { $1 : $2 }
  | type_qualifier %prec NAMED                       { [$1] }
  | type_qualifier declaration_specifiers_           { $1 : $2 }

-- | This production allows us to add storage class specifiers and type
-- qualifiers to an anti-quoted type.

storage_qualifier_specifiers :: { [TySpec] }
storage_qualifier_specifiers :
    storage_class_specifier %prec NAMED                   { [$1]}
  | storage_class_specifier storage_qualifier_specifiers  { $1 : $2 }
  | type_qualifier %prec NAMED                            { [$1] }
  | type_qualifier storage_qualifier_specifiers           { $1 : $2 }

init_declarator_list :: { RevList Init }
init_declarator_list :
    init_declarator                           { rsingleton $1 }
  | init_declarator_list ',' init_declarator  { rcons $3 $1 }

maybe_asmlabel :: { Maybe AsmLabel }
maybe_asmlabel :
     {- empty -}             { Nothing }
  | '__asm__' '(' STRING ')' { Just ((fst . getSTRING) $3) }

init_declarator :: { Init }
init_declarator :
    declarator maybe_asmlabel
      { let  {  (ident, declToDecl) = $1
             ;  decl                = declToDecl (declRoot ident)
             }
        in
          Init ident decl $2 Nothing [] (ident `srcspan` decl)
      }
  | declarator attributes maybe_asmlabel
      { let  { (ident, declToDecl) = $1
             ;  decl               = declToDecl (declRoot ident)
             }
        in
          Init ident decl $3 Nothing $2 (ident `srcspan` decl)
      }
  | declarator maybe_asmlabel '=' initializer
      { let  {  (ident, declToDecl) = $1
             ;  decl                = declToDecl (declRoot ident)
             }
        in
          Init ident decl $2 (Just $4) [] (ident `srcspan` $4)
      }
  | declarator attributes maybe_asmlabel '=' initializer
      { let  {  (ident, declToDecl) = $1
             ;  decl                = declToDecl (declRoot ident)
             }
        in
          Init ident decl $3 (Just $5) $2 (ident `srcspan` $5)
      }
  | declarator error
      {% do{  let (ident, declToDecl) = $1
           ;  let decl                = declToDecl (declRoot ident)
           ;  expected ["'='"]
           }
      }

storage_class_specifier :: { TySpec }
storage_class_specifier :
    'auto'           { TSauto (srclocOf $1) }
  | 'register'       { TSregister (srclocOf $1) }
  | 'static'         { TSstatic (srclocOf $1) }
  | 'extern'         { TSextern (srclocOf $1) }
  | 'extern' STRING  { TSexternL ((snd . getSTRING) $2) (srclocOf $1) }
  | 'typedef'        { TStypedef (srclocOf $1) }

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

  {- Extension: GCC -}
  | '__builtin_va_list' { TSva_list (srclocOf $1) }

struct_or_union_specifier :: { TySpec }
struct_or_union_specifier :
    struct_or_union identifier_or_typedef
      { (unLoc $1) (Just $2) Nothing [] ($1 `srcspan` $2) }
  | struct_or_union attributes identifier_or_typedef
      { (unLoc $1) (Just $3) Nothing $2 ($1 `srcspan` $3) }
  | struct_or_union '{' struct_declaration_list '}'
      { (unLoc $1) Nothing (Just (rev $3)) [] ($1 `srcspan` $4) }
  | struct_or_union '{' struct_declaration_list error
      {% unclosed ($1 <--> rev $3) "{" }
  | struct_or_union identifier_or_typedef '{' struct_declaration_list '}'
      { (unLoc $1) (Just $2) (Just (rev $4)) [] ($1 `srcspan` $5) }
  | struct_or_union identifier_or_typedef '{' struct_declaration_list error
      {% unclosed ($1 <--> rev $4) "{" }
  | struct_or_union attributes identifier_or_typedef '{' struct_declaration_list '}'
      { (unLoc $1) (Just $3) (Just (rev $5)) $2 ($1 `srcspan` $6) }
  | struct_or_union attributes '{' struct_declaration_list '}'
      { (unLoc $1) Nothing (Just (rev $4)) $2 ($1 `srcspan` $5) }
  | struct_or_union attributes identifier_or_typedef '{' struct_declaration_list error
      {% unclosed ($1 <--> rev $5) "{" }

struct_or_union :: { L (Maybe Id -> Maybe [FieldGroup] -> [Attr] -> SrcLoc -> TySpec) }
struct_or_union :
    'struct' { L (locOf $1) TSstruct }
  | 'union'  { L (locOf $1) TSunion }

struct_declaration_list :: { RevList FieldGroup }
struct_declaration_list :
    struct_declaration
      { rsingleton $1 }
  | ANTI_SDECLS
      { rsingleton (AntiSdecls (getANTI_SDECLS $1) (srclocOf $1)) }
  | struct_declaration_list struct_declaration
      { rcons $2 $1 }
  | struct_declaration_list ANTI_SDECLS
      { rcons (AntiSdecls (getANTI_SDECLS $2) (srclocOf $2)) $1 }

struct_declaration :: { FieldGroup }
struct_declaration :
    ANTI_SPEC struct_declarator_list ';'
      { let dspec = AntiDeclSpec (getANTI_SPEC $1) (srclocOf $1)
        in
          FieldGroup dspec (rev $2) ($1 `srcspan` $3)
      }
  | specifier_qualifier_list struct_declarator_list ';'
      {%  do{ dspec <- mkDeclSpec $1
            ; return $ FieldGroup dspec (rev $2) ($1 `srcspan` $3)
            }
      }
  | ANTI_TYPE identifier_or_typedef ';'
      {%  do{ let v     = getANTI_TYPE $1
            ; let dspec = AntiTypeDeclSpec [] [] v (srclocOf $1)
            ; let decl  = AntiTypeDecl v (srclocOf $1)
            ; let field = Field (Just $2) (Just decl) Nothing ($1 `srcspan` $2)
            ; return $ FieldGroup dspec [field] ($1 `srcspan` $3)
            }
      }
  | ANTI_SDECL
      { AntiSdecl (getANTI_SDECL $1) (srclocOf $1) }

specifier_qualifier_list :: { [TySpec] }
specifier_qualifier_list :
    type_specifier specifier_qualifier_list_
      { $1 : $2 }
  | type_qualifier_list type_specifier specifier_qualifier_list_
      { rev $1 ++ [$2] ++ $3 }
  | typedef_name
      { [$1] }
  | typedef_name type_qualifier_list
      { $1 : rev $2 }
  | type_qualifier_list typedef_name
      { rev $1 ++ [$2] }
  | type_qualifier_list typedef_name type_qualifier_list
      { rev $1 ++ [$2] ++ rev $3 }

specifier_qualifier_list_ :: { [TySpec] }
specifier_qualifier_list_ :
    {- empty -} %prec NAMED                  { [] }
  | type_specifier %prec NAMED               { [$1] }
  | type_specifier specifier_qualifier_list  { $1 : $2 }
  | type_qualifier %prec NAMED               { [$1] }
  | type_qualifier specifier_qualifier_list  { $1 : $2 }

struct_declarator_list :: { RevList Field }
struct_declarator_list :
    struct_declarator                            { rsingleton $1 }
  | struct_declarator_list ',' struct_declarator { rcons $3 $1 }

struct_declarator :: { Field }
struct_declarator :
    declarator
        { let { (ident, declToDecl) = $1
              ; decl                = declToDecl (declRoot ident)
              }
          in
            Field (Just ident) (Just decl) Nothing (srclocOf decl)
        }
  | declarator attributes
        { let { (ident, declToDecl) = $1
              ; decl                = declToDecl (declRoot ident)
              }
          in
            Field (Just ident) (Just decl) Nothing (srclocOf decl)
        }
  | ':' constant_expression
        { Field Nothing Nothing (Just $2) ($1 `srcspan` $2) }
  | declarator ':' constant_expression
        { let { (ident, declToDecl) = $1
              ; decl                = declToDecl (declRoot ident)
              }
          in
            Field (Just ident) (Just decl) (Just $3) (srclocOf decl)
        }

enum_specifier :: { TySpec }
enum_specifier :
    'enum' identifier_or_typedef
      { TSenum (Just $2) [] [] ($1 `srcspan` $2) }
  | 'enum' attributes identifier_or_typedef
      { TSenum (Just $3) [] $2 ($1 `srcspan` $3) }
  | 'enum' '{' enumerator_list '}'
      { TSenum Nothing (rev $3) [] ($1 `srcspan` $4) }
  | 'enum' identifier_or_typedef '{' enumerator_list '}'
      { TSenum (Just $2) (rev $4) [] ($1 `srcspan` $5)}

enumerator_list :: { RevList CEnum }
enumerator_list :
    enumerator
      { rsingleton $1 }
  | ANTI_ENUMS
      { rsingleton (AntiEnums (getANTI_ENUMS $1) (srclocOf $1)) }
  | enumerator_list ','
      { $1 }
  | enumerator_list ',' enumerator
      { rcons $3 $1 }
  | enumerator_list ',' ANTI_ENUMS
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
  | 'inline'   { TSinline (srclocOf $1) }
  | 'restrict' { TSrestrict (srclocOf $1) }
  | 'volatile' { TSvolatile (srclocOf $1) }

  {- Extension: CUDA -}
  | '__device__'   { TSCUDAdevice (srclocOf $1) }
  | '__global__'   { TSCUDAglobal (srclocOf $1) }
  | '__host__'     { TSCUDAhost (srclocOf $1) }
  | '__constant__' { TSCUDAconstant (srclocOf $1) }
  | '__shared__'   { TSCUDAshared (srclocOf $1) }
  | '__restrict__' { TSCUDArestrict (srclocOf $1) }
  | '__noinline__' { TSCUDAnoinline (srclocOf $1) }

  {- Extension: OpenCL -}
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
-- is an item in a parmeter list for a function that is part of an abstract
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
  | identifier_direct_declarator '(' identifier_list ')'
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
  | typedef_direct_declarator '(' identifier_list ')'
      { let { (ident, declToDecl) = $1
            ; proto = mkOldProto (rev $3)
            }
        in
          (ident, declToDecl . proto)
      }

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
  | parameter_typedef_direct_declarator '(' identifier_list ')'
      { let { (ident, declToDecl) = $1
            ; proto = mkOldProto (rev $3)
            }
        in
          (ident, declToDecl . proto)
      }

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
  | '[' type_qualifier_list ']'
      { mkArray (rev $2) (NoArraySize ($1 `srcspan` $3)) }
  | '[' assignment_expression ']'
      { mkArray [] (ArraySize False $2 (srclocOf $2)) }
  | '[' type_qualifier_list assignment_expression ']'
      { mkArray (rev $2) (ArraySize False $3 (srclocOf $3)) }
  | '[' 'static' assignment_expression ']'
      { mkArray [] (ArraySize True $3 (srclocOf $3)) }
  | '[' 'static' type_qualifier_list assignment_expression ']'
      { mkArray (rev $3) (ArraySize True $4 (srclocOf $4)) }
  | '[' type_qualifier_list 'static' assignment_expression ']'
      { mkArray (rev $2) (ArraySize True $4 (srclocOf $4)) }
  | '[' '*' ']'
      { mkArray [] (VariableArraySize ($1 `srcspan` $3)) }
  | '[' type_qualifier_list  '*' ']'
      { mkArray (rev $2) (VariableArraySize ($1 `srcspan` $4)) }

pointer :: { Decl -> Decl }
pointer :
    '*'                              { mkPtr [] }
  | '*' type_qualifier_list          { mkPtr (rev $2) }
  | '*' pointer                      { $2 . mkPtr [] }
  | '*' type_qualifier_list pointer  { $3 . mkPtr (rev $2) }

type_qualifier_list :: { RevList TySpec }
type_qualifier_list :
    type_qualifier                     { rsingleton $1 }
  | type_qualifier_list type_qualifier { rcons $2 $1 }

parameter_type_list :: { Params }
parameter_type_list :
    parameter_list
      { let params = rev $1
        in
          Params params False (srclocOf params)
      }
  | parameter_list ',' '...'
      { let params = rev $1
        in
          Params params True (params `srcspan` $3)
      }

parameter_list :: { RevList Param }
parameter_list:
    parameter_declaration
      { rsingleton $1 }
  | ANTI_PARAMS
      { rsingleton (AntiParams (getANTI_PARAMS $1) (srclocOf $1)) }
  | parameter_list ',' parameter_declaration
      { rcons $3 $1 }
  | parameter_list ',' ANTI_PARAMS
      { rcons (AntiParams (getANTI_PARAMS $3) (srclocOf $3))  $1 }

parameter_declaration :: { Param }
parameter_declaration:
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
type_declaration:
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

identifier_list :: { RevList Id }
identifier_list :
    identifier                      { rsingleton $1 }
  | identifier_list ',' identifier  { rcons $3 $1 }

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
  | ANTI_TYPE abstract_declarator
      { let  {  v     = getANTI_TYPE $1
             ;  decl  = $2 (AntiTypeDecl v (srclocOf $1))
             }
        in
          Type (AntiTypeDeclSpec [] [] v (srclocOf $1)) decl ($1 `srcspan` decl)
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
      { TSnamed (Id (getNAMED $1) (srclocOf $1)) (srclocOf $1) }
  | 'typename' identifier
      { TSnamed $2 ($1 `srcspan` $2) }
  | 'typename' error
      {% expected ["identifier"] }
  | '__typeof__' '(' unary_expression ')'
      { TStypeofExp $3 ($1 `srcspan` $4) }
  | '__typeof__' '(' type_name ')'
      { TStypeofType $3 ($1 `srcspan` $4) }
  | '__typeof__' '(' type_name error
      {% unclosed ($2 <--> $3) "(" }

initializer :: { Initializer }
initializer :
    assignment_expression
      { ExpInitializer $1 (srclocOf $1) }
  | '{' initializer_list '}'
      { CompoundInitializer (rev $2) ($1 `srcspan` $3) }
  | '{' initializer_list error
      {% do{  let (_, inits) = unzip (rev $2)
           ;  unclosed ($1 <--> inits) "{"
           }
      }
  | '{' initializer_list ',' '}'
      { CompoundInitializer (rev $2) ($1 `srcspan` $4) }
  | '{' initializer_list ',' error
      {% unclosed ($1 <--> $3) "{" }

initializer_list :: { RevList (Maybe Designation, Initializer) }
initializer_list :
    initializer
      { rsingleton (Nothing, $1) }
  | designation initializer
      { rsingleton (Just $1, $2) }
  | initializer_list ',' initializer
      { rcons (Nothing, $3) $1 }
  | initializer_list ',' designation initializer
      { rcons (Just $3, $4) $1 }

designation :: { Designation }
designation :
    designator_list '='
      { let designators = rev $1
        in
          Designation designators (designators `srcspan` $2)
      }

designator_list :: { RevList Designator }
designator_list :
    designator                 { rsingleton $1 }
  | designator_list designator { rcons $2 $1 }

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
    labeled_statement    { $1 }
  | compound_statement   { $1 }
  | expression_statement { $1 }
  | selection_statement  { $1 }
  | iteration_statement  { $1 }
  | jump_statement       { $1 }
  | asm_statement        { $1 }
  | '#pragma'            { Pragma (getPRAGMA $1) (srclocOf $1) }
  | ANTI_PRAGMA          { AntiPragma (getANTI_PRAGMA $1) (srclocOf $1) }
  | ANTI_STM             { AntiStm (getANTI_STM $1) (srclocOf $1) }

labeled_statement :: { Stm }
labeled_statement :
    identifier ':' error                      {% expected ["statement"] }
  | identifier ':' statement                  { Label $1 $3 ($1 `srcspan` $3) }
  | 'case' constant_expression error          {% expected ["`:'"] }
  | 'case' constant_expression ':' error      {% expected ["statement"] }
  | 'case' constant_expression ':' statement  { Case $2 $4 ($1 `srcspan` $4) }
  | 'default' error                           {% expected ["`:'"] }
  | 'default' ':' error                       {% expected ["statement"] }
  | 'default' ':' statement                   { Default $3 ($1 `srcspan` $3) }

compound_statement :: { Stm }
compound_statement:
    '{' begin_scope end_scope '}'
      { Block [] ($1 `srcspan` $4) }
  | '{' begin_scope error
      {% unclosed (locOf $3) "{" }
  | '{' begin_scope block_item_list end_scope '}'
      { Block (rev $3) ($1 `srcspan` $5) }

block_item_list :: { RevList BlockItem }
block_item_list :
     block_item                 { rsingleton $1 }
  |  block_item_list block_item { rcons $2 $1 }

block_item  :: { BlockItem }
block_item :
     declaration { BlockDecl $1 }
  |  statement   { BlockStm $1 }
  |  ANTI_DECLS  { BlockDecl (AntiDecls (getANTI_DECLS $1) (srclocOf $1)) }
  |  ANTI_STMS   { BlockStm (AntiStms (getANTI_STMS $1) (srclocOf $1)) }
  |  ANTI_ITEM   { AntiBlockItem (getANTI_ITEM $1) (srclocOf $1) }
  |  ANTI_ITEMS  { AntiBlockItems (getANTI_ITEMS $1)  (srclocOf $1) }

begin_scope :: { () }
begin_scope : {% pushScope }

end_scope :: { () }
end_scope : {% popScope }

declaration_list :: { RevList InitGroup }
declaration_list :
    declaration
      { rsingleton $1 }
  | ANTI_DECLS
      { rsingleton (AntiDecls (getANTI_DECLS $1) (srclocOf $1)) }
  | declaration_list declaration
      { rcons $2 $1 }
  | declaration_list ANTI_DECLS
      { rcons (AntiDecls (getANTI_DECLS $2) (srclocOf $2)) $1 }

expression_statement :: { Stm }
expression_statement:
    ';'               { Exp Nothing (srclocOf $1) }
  | expression ';'    { Exp (Just $1) ($1 `srcspan` $2) }
  | expression error  {% expected ["';'"] }

selection_statement :: { Stm }
selection_statement :
    'if' '(' expression ')' statement
      { If $3 $5 Nothing ($1 `srcspan` $5) }
  | 'if' '(' expression ')' statement 'else' statement
      { If $3 $5 (Just $7) ($1 `srcspan` $7) }
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
      {% expected ["expression", "declaration"] }
  | 'for' '(' declaration maybe_expression ';' ')' statement
      { For (Left $3) $4 Nothing $7 ($1 `srcspan` $7) }
  | 'for' '(' maybe_expression ';' maybe_expression ';' ')' statement
      { For (Right $3) $5 Nothing $8 ($1 `srcspan` $8) }
  | 'for' '(' maybe_expression ';' maybe_expression ';' error
      {% unclosed ($2 <--> $6) "(" }
  | 'for' '(' declaration maybe_expression ';' expression ')' statement
      { For (Left $3) $4 (Just $6) $8 ($1 `srcspan` $8) }
  | 'for' '(' maybe_expression ';' maybe_expression ';' expression ')' statement
      { For (Right $3) $5 (Just $7) $9 ($1 `srcspan` $9) }
  | 'for' '(' maybe_expression ';' maybe_expression ';' expression error
      {% unclosed ($2 <--> $7) "(" }

jump_statement :: { Stm }
jump_statement :
    'goto' identifier ';'      { Goto $2 ($1 `srcspan` $3) }
  | 'goto' error               {% expected ["identifier"] }
  | 'goto' identifier error    {% expected ["';'"] }
  | 'continue' ';'             { Continue ($1 `srcspan` $2) }
  | 'continue' error           {% expected ["';'"] }
  | 'break' ';'                { Break ($1 `srcspan` $2) }
  | 'break' error              {% expected ["';'"] }
  | 'return' ';'               { Return Nothing ($1 `srcspan` $2) }
  | 'return' error             {% expected ["';'"] }
  | 'return' expression ';'    { Return (Just $2) ($1 `srcspan` $3) }
  | 'return' expression error  {% expected ["';'"] }

{------------------------------------------------------------------------------
 -
 - External definitions
 -
 ------------------------------------------------------------------------------}

translation_unit :: { [Definition] }
translation_unit :
    {- empty -}       { [] }
  | translation_unit_ { rev $1 }

translation_unit_ :: { RevList Definition }
translation_unit_ :
    external_declaration
      { rsingleton $1 }
  | ANTI_EDECLS
      { rsingleton (AntiEdecls (getANTI_EDECLS $1) (srclocOf $1)) }
  | translation_unit_ external_declaration
      { rcons $2 $1 }
  | translation_unit_ ANTI_EDECLS
      { rcons (AntiEdecls (getANTI_EDECLS $2) (srclocOf $2)) $1 }

external_declaration :: { Definition }
external_declaration :
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
           ; let Block blockItems _  =  $3
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
  | declaration_specifiers declarator declaration_list compound_statement
      {% do{ let (dspec, declRoot)   =  $1
           ; let (ident, declToDecl) =  $2
           ; let argDecls            =  $3
           ; let Block blockItems _  =  $4
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

{------------------------------------------------------------------------------
 -
 - GCC extensions
 -
 ------------------------------------------------------------------------------}

attributes :: { [Attr] }
attributes :
    attribute            { $1 }
  | attributes attribute { $1 ++ $2 }

attribute :: { [Attr] }
attribute :
    '__attribute__' '(' '(' attribute_list ')' ')' { rev $4 }

attribute_list :: { RevList Attr }
attribute_list :
    attrib                    { rsingleton $1 }
  | attribute_list ',' attrib { rcons $3 $1 }

attrib :: { Attr }
attrib :
    attrib_name
      { Attr $1 [] (srclocOf $1)}
  | attrib_name '(' argument_expression_list ')'
      { Attr $1 (rev $3) ($1 `srcspan` $4) }

attrib_name :: { Id }
attrib_name :
    identifier_or_typedef { $1 }
  | 'static'              { Id "static" (srclocOf $1) }
  | 'extern'              { Id "extern" (srclocOf $1) }
  | 'register'            { Id "register" (srclocOf $1) }
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
    '__asm__' maybe_volatile '(' asm_template ')' ';'
      { Asm $2 [] (rev $4) [] [] [] ($1 `srcspan` $5) }
  | '__asm__' maybe_volatile '(' asm_template ':' asm_inouts ')' ';'
      { Asm $2 [] (rev $4) $6 [] [] ($1 `srcspan` $7) }
  | '__asm__' maybe_volatile '(' asm_template ':' asm_inouts
                                              ':' asm_inouts ')' ';'
      { Asm $2 [] (rev $4) $6 $8 [] ($1 `srcspan` $9) }
  | '__asm__' maybe_volatile '(' asm_template ':' asm_inouts
                                              ':' asm_inouts
                                              ':' asm_clobbers ')' ';'
      { Asm $2 [] (rev $4) $6 $8 $10 ($1 `srcspan` $11) }

asm_template :: { RevList String }
asm_template :
    STRING               { rsingleton ((fst . getSTRING) $1) }
  | asm_template STRING  { rcons ((fst . getSTRING) $2) $1 }

asm_inouts :: { [(String, Exp)] }
asm_inouts :
    {- empty -}    { [] }
  | asm_inouts_ne  { rev $1 }

asm_inouts_ne :: { RevList (String, Exp) }
asm_inouts_ne:
    asm_inout                    { rsingleton $1 }
  | asm_inouts_ne ',' asm_inout  { rcons $3 $1 }

asm_inout :: { (String, Exp) }
asm_inout :
    STRING '(' expression ')' { ((fst . getSTRING) $1, $3) }

asm_clobbers :: { [String] }
asm_clobbers :
    {- empty -}      { [] }
  | asm_clobbers_ne  { rev $1 }

asm_clobbers_ne :: { RevList String }
asm_clobbers_ne :
    asm_clobber                      { rsingleton $1 }
  | asm_clobbers_ne ',' asm_clobber  { rcons $3 $1 }

asm_clobber :: { String }
asm_clobber :
    STRING { (fst . getSTRING) $1 }

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

getPRAGMA      (L _ (T.Tpragma pragma))      = pragma

getANTI_ID          (L _ (T.Tanti_id v))          = v
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
getANTI_EDECL       (L _ (T.Tanti_edecl v))       = v
getANTI_EDECLS      (L _ (T.Tanti_edecls v))      = v
getANTI_ITEM        (L _ (T.Tanti_item v))        = v
getANTI_ITEMS       (L _ (T.Tanti_items v))       = v
getANTI_STM         (L _ (T.Tanti_stm v))         = v
getANTI_STMS        (L _ (T.Tanti_stms v))        = v
getANTI_TYPE        (L _ (T.Tanti_type v))        = v
getANTI_SPEC        (L _ (T.Tanti_spec v))        = v
getANTI_PARAM       (L _ (T.Tanti_param v))       = v
getANTI_PARAMS      (L _ (T.Tanti_params v))      = v
getANTI_PRAGMA      (L _ (T.Tanti_pragma v))      = v

lexer :: (L T.Token -> P a) -> P a
lexer cont = do
    t <- lexToken
    setCurToken t
    cont t

locate :: Loc -> (SrcLoc -> a) -> L a
locate loc f = L loc (f (SrcLoc loc))

data DeclTySpec = DeclTySpec DeclSpec !SrcLoc
                | AntiDeclTySpec String !SrcLoc

data TySpec = TSauto !SrcLoc
            | TSregister !SrcLoc
            | TSstatic !SrcLoc
            | TSextern !SrcLoc
            | TSexternL String !SrcLoc
            | TStypedef !SrcLoc

            | TSconst !SrcLoc
            | TSvolatile !SrcLoc
            | TSinline !SrcLoc

            | TSsigned !SrcLoc
            | TSunsigned !SrcLoc

            | TSvoid !SrcLoc
            | TSchar !SrcLoc
            | TSshort !SrcLoc
            | TSint !SrcLoc
            | TSlong !SrcLoc
            | TSfloat !SrcLoc
            | TSdouble !SrcLoc

            | TSstruct (Maybe Id) (Maybe [FieldGroup]) [Attr] !SrcLoc
            | TSunion (Maybe Id) (Maybe [FieldGroup]) [Attr] !SrcLoc
            | TSenum (Maybe Id) [CEnum] [Attr] !SrcLoc
            | TSnamed Id !SrcLoc

            | TStypeofExp Exp !SrcLoc
            | TStypeofType Type !SrcLoc
            | TSva_list !SrcLoc

            -- C99
            | TSrestrict !SrcLoc

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

instance Located DeclTySpec where
    locOf (DeclTySpec _ loc)      = locOf loc
    locOf (AntiDeclTySpec _ loc)  = locOf loc

instance Located TySpec where
    locOf (TSauto loc)          = locOf loc
    locOf (TSregister loc)      = locOf loc
    locOf (TSstatic loc)        = locOf loc
    locOf (TSextern loc)        = locOf loc
    locOf (TSexternL _ loc)     = locOf loc
    locOf (TStypedef loc)       = locOf loc

    locOf (TSconst loc)         = locOf loc
    locOf (TSvolatile loc)      = locOf loc
    locOf (TSinline loc)        = locOf loc

    locOf (TSsigned loc)        = locOf loc
    locOf (TSunsigned loc)      = locOf loc

    locOf (TSvoid loc)          = locOf loc
    locOf (TSchar loc)          = locOf loc
    locOf (TSshort loc)         = locOf loc
    locOf (TSint loc)           = locOf loc
    locOf (TSlong loc)          = locOf loc
    locOf (TSfloat loc)         = locOf loc
    locOf (TSdouble loc)        = locOf loc

    locOf (TSstruct _ _ _ loc)  = locOf loc
    locOf (TSunion _ _ _ loc)   = locOf loc
    locOf (TSenum _ _ _ loc)    = locOf loc
    locOf (TSnamed _ loc)       = locOf loc

    locOf (TStypeofExp _ loc)   = locOf loc
    locOf (TStypeofType _ loc)  = locOf loc
    locOf (TSva_list loc)       = locOf loc

    locOf (TSrestrict loc)      = locOf loc

    locOf (TSCUDAdevice loc)    = locOf loc
    locOf (TSCUDAglobal loc)    = locOf loc
    locOf (TSCUDAhost loc)      = locOf loc
    locOf (TSCUDAconstant loc)  = locOf loc
    locOf (TSCUDAshared loc)    = locOf loc
    locOf (TSCUDArestrict loc)  = locOf loc
    locOf (TSCUDAnoinline loc)  = locOf loc

    locOf (TSCLprivate loc)     = locOf loc
    locOf (TSCLlocal loc)       = locOf loc
    locOf (TSCLglobal loc)      = locOf loc
    locOf (TSCLconstant loc)    = locOf loc
    locOf (TSCLreadonly loc)    = locOf loc
    locOf (TSCLwriteonly loc)   = locOf loc
    locOf (TSCLkernel loc)      = locOf loc

instance Pretty TySpec where
    ppr (TSauto _)       = text "auto"
    ppr (TSregister _)   = text "register"
    ppr (TSstatic _)     = text "static"
    ppr (TSextern _)     = text "extern"
    ppr (TSexternL l _)  = text "extern" <+> ppr l
    ppr (TStypedef _)    = text "typedef"

    ppr (TSconst _)    = text "const"
    ppr (TSinline _)   = text "inline"
    ppr (TSrestrict _) = text "restrict"
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

    ppr (TStypeofExp e _)   = text "__typeof__" <> parens (ppr e)
    ppr (TStypeofType ty _) = text "__typeof__" <> parens (ppr ty)
    ppr (TSnamed ident _)   = ppr ident

    ppr (TSva_list _)   = text "__builtin_va_list"

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
isStorage (TSauto _)      = True
isStorage (TSregister _)  = True
isStorage (TSstatic _)    = True
isStorage (TSextern _)    = True
isStorage (TSexternL _ _) = True
isStorage (TStypedef _)   = True
isStorage _               = False

mkStorage :: [TySpec] -> [Storage]
mkStorage specs = map mk (filter isStorage specs)
    where
      mk :: TySpec -> Storage
      mk (TSauto loc)      = Tauto loc
      mk (TSregister loc)  = Tregister loc
      mk (TSstatic loc)    = Tstatic loc
      mk (TSextern loc)    = Textern loc
      mk (TSexternL l loc) = TexternL l loc
      mk (TStypedef loc)   = Ttypedef loc
      mk _                 = error "internal error in mkStorage"

isTypeQual :: TySpec -> Bool
isTypeQual (TSconst _)        = True
isTypeQual (TSvolatile _)     = True
isTypeQual (TSinline _)       = True
isTypeQual (TSrestrict _)     = True
isTypeQual (TSCUDAdevice _)   = True
isTypeQual (TSCUDAglobal _)   = True
isTypeQual (TSCUDAhost _)     = True
isTypeQual (TSCUDAconstant _) = True
isTypeQual (TSCUDAshared _)   = True
isTypeQual (TSCUDArestrict _) = True
isTypeQual (TSCUDAnoinline _) = True
isTypeQual (TSCLprivate _)    = True
isTypeQual (TSCLlocal _)      = True
isTypeQual (TSCLglobal _)     = True
isTypeQual (TSCLconstant _)   = True
isTypeQual (TSCLreadonly _)   = True
isTypeQual (TSCLwriteonly _)  = True
isTypeQual (TSCLkernel _)     = True
isTypeQual _                  = False

mkTypeQuals :: [TySpec] -> [TypeQual]
mkTypeQuals specs = map mk (filter isTypeQual specs)
    where
      mk :: TySpec -> TypeQual
      mk (TSconst loc)        = Tconst loc
      mk (TSvolatile loc)     = Tvolatile loc
      mk (TSinline loc)       = Tinline loc
      mk (TSrestrict loc)     = Trestrict loc
      mk (TSCUDAdevice loc)   = TCUDAdevice loc
      mk (TSCUDAglobal loc)   = TCUDAglobal loc
      mk (TSCUDAhost loc)     = TCUDAhost loc
      mk (TSCUDAconstant loc) = TCUDAconstant loc
      mk (TSCUDAshared loc)   = TCUDAshared loc
      mk (TSCUDArestrict loc) = TCUDArestrict loc
      mk (TSCUDAnoinline loc) = TCUDAnoinline loc
      mk (TSCLprivate loc)    = TCLprivate loc
      mk (TSCLlocal loc)      = TCLlocal loc
      mk (TSCLglobal loc)     = TCLglobal loc
      mk (TSCLconstant loc)   = TCLconstant loc
      mk (TSCLreadonly loc)   = TCLreadonly loc
      mk (TSCLwriteonly loc)  = TCLwriteonly loc
      mk (TSCLkernel loc)     = TCLkernel loc
      mk _                    = error "internal error in mkTypeQual"

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

composeDecls :: Decl -> Decl -> Decl
composeDecls (DeclRoot _) root =
    root

composeDecls (C.Ptr quals decl loc) root =
    C.Ptr quals (composeDecls decl root) loc

composeDecls (Array quals size decl loc) root =
    Array quals size (composeDecls decl root) loc

composeDecls (Proto decl args loc) root =
    Proto (composeDecls decl root) args loc

composeDecls (OldProto decl args loc) root =
    OldProto (composeDecls decl root) args loc

mkDeclSpec :: [TySpec] -> P DeclSpec
mkDeclSpec specs =
    go rest
  where
    storage ::[Storage]
    storage = mkStorage specs

    quals :: [TypeQual]
    quals = mkTypeQuals specs

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

    go [TSint _, TSshort _] = do
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

    go [TSint _, TSlong _] = do
        sign <- mkSign specs
        return $ cdeclSpec storage quals (Tlong sign (srclocOf rest))

    go [TSlong _, TSlong _] = do
        sign <- mkSign specs
        return $ cdeclSpec storage quals (Tlong_long sign (srclocOf rest))

    go [TSlong _, TSlong _, TSint _] = do
        sign <- mkSign specs
        return $ cdeclSpec storage quals (Tlong_long sign (srclocOf rest))

    go [TSlong _, TSint _, TSlong _] = do
        sign <- mkSign specs
        return $ cdeclSpec storage quals (Tlong_long sign (srclocOf rest))

    go [TSint _, TSlong _, TSlong _] = do
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

    go [TSdouble _, TSlong _] = do
        checkNoSign specs "sign specified for long double type"
        return $ cdeclSpec storage quals (Tlong_double (srclocOf rest))

    go [TSstruct ident fields attrs loc] = do
        checkNoSign specs "sign specified for struct type"
        return $ cdeclSpec storage quals (Tstruct ident fields attrs loc)

    go [TSunion ident fields attrs loc] = do
        checkNoSign specs "sign specified for union type"
        return $ cdeclSpec storage quals (Tunion ident fields attrs loc)

    go [TSenum ident enums attrs loc] = do
        checkNoSign specs "sign specified for enum type"
        return $ cdeclSpec storage quals (Tenum ident enums attrs loc)

    go [TSnamed ident loc] = do
        checkNoSign specs "sign specified for named type"
        return $ cdeclSpec storage quals (Tnamed ident loc)

    go [TStypeofExp e loc] = do
        checkNoSign specs "sign specified for typeof"
        return $ cdeclSpec storage quals (TtypeofExp e loc)

    go [TStypeofType ty loc] = do
        checkNoSign specs "sign specified for typeof"
        return $ cdeclSpec storage quals (TtypeofType ty loc)

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

declRoot :: Located a => a -> Decl
declRoot x = DeclRoot (srclocOf x)

data RevList a  =  RNil
                |  RCons a (RevList a)

rnil :: RevList a
rnil = RNil

rsingleton :: a -> RevList a
rsingleton x = RCons x RNil

rcons :: a -> RevList a -> RevList a
rcons x xs  = RCons x xs

rev :: RevList a -> [a]
rev xs = go [] xs
  where
    go  l  RNil          = l
    go  l  (RCons x xs)  = go (x : l) xs
}
