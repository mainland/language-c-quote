{
{-# OPTIONS -w #-}
{-# LANGUAGE CPP #-}

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
-- Module      :  Language.C.Parser.Lexer
-- Copyright   :  (c) Harvard University 2006-2010
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

module Language.C.Parser.Lexer (
    lexToken
  ) where

import Control.Monad (when)
import Control.Monad.Error
import Control.Monad.Exception
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Trans
import qualified Data.ByteString.Char8 as B
import Data.Char (isAlphaNum,
                  isDigit,
                  isOctDigit,
                  isHexDigit,
                  isLower,
                  isSpace,
                  chr,
                  toLower)
import Data.List (foldl', intersperse)
import Data.Loc
import qualified Data.Map as Map
import Data.Ratio ((%))
import qualified Data.Set as Set
import Data.Symbol
import Data.Maybe (fromMaybe)
import Text.PrettyPrint.Mainland

import qualified Language.C.Syntax as C
import Language.C.Parser.Monad
import Language.C.Parser.Tokens
}

$nondigit         = [a-z A-Z \_]
$digit            = [0-9]
$nonzerodigit     = [1-9]
$octalDigit       = [0-7]
$hexadecimalDigit = [0-9A-Fa-f]

@fractionalConstant = $digit* "." $digit+
                    | $digit+ "."
@exponentPart       = [eE] [\+\-]? $digit+

@floatingSuffix     = [fF]
                    | [lL]

@floatingConstant   = @fractionalConstant @exponentPart? @floatingSuffix?
                    | $digit+ @exponentPart @floatingSuffix?

@decimalConstant     = $nonzerodigit $digit*
@octalConstant       = "0" $octalDigit*
@hexadecimalConstant = "0" [xX] $hexadecimalDigit+

@integerSuffix = [uU] [lL]?
               | [lL] [uU]?
               | [lL] [lL] [uU]?
               | [uU] [lL] [lL]

$whitechar = [\ \t\n\r\f\v]

c :-

<qq> {
 "typename"  / { allowAnti } { token Ttypename }

 "$id:"      / { allowAnti } { lexAnti Tanti_id }
 "$int:"     / { allowAnti } { lexAnti Tanti_int }
 "$uint:"    / { allowAnti } { lexAnti Tanti_uint }
 "$lint:"    / { allowAnti } { lexAnti Tanti_lint }
 "$ulint:"   / { allowAnti } { lexAnti Tanti_ulint }
 "$float:"   / { allowAnti } { lexAnti Tanti_float }
 "$double:"  / { allowAnti } { lexAnti Tanti_double }
 "$ldouble:" / { allowAnti } { lexAnti Tanti_long_double }
 "$char:"    / { allowAnti } { lexAnti Tanti_char }
 "$string:"  / { allowAnti } { lexAnti Tanti_string }
 "$exp:"     / { allowAnti } { lexAnti Tanti_exp }
 "$func:"    / { allowAnti } { lexAnti Tanti_func }
 "$args:"    / { allowAnti } { lexAnti Tanti_args }
 "$decl:"    / { allowAnti } { lexAnti Tanti_decl }
 "$decls:"   / { allowAnti } { lexAnti Tanti_decls }
 "$sdecl:"   / { allowAnti } { lexAnti Tanti_sdecl }
 "$sdecls:"  / { allowAnti } { lexAnti Tanti_sdecls }
 "$enum:"    / { allowAnti } { lexAnti Tanti_enum }
 "$enums:"   / { allowAnti } { lexAnti Tanti_enums }
 "$esc:"     / { allowAnti } { lexAnti Tanti_esc }
 "$edecl:"   / { allowAnti } { lexAnti Tanti_edecl }
 "$edecls:"  / { allowAnti } { lexAnti Tanti_edecls }
 "$item:"    / { allowAnti } { lexAnti Tanti_item }
 "$items:"   / { allowAnti } { lexAnti Tanti_items }
 "$stm:"     / { allowAnti } { lexAnti Tanti_stm }
 "$stms:"    / { allowAnti } { lexAnti Tanti_stms }
 "$ty:"      / { allowAnti } { lexAnti Tanti_type }
 "$spec:"    / { allowAnti } { lexAnti Tanti_spec }
 "$param:"   / { allowAnti } { lexAnti Tanti_param }
 "$params:"  / { allowAnti } { lexAnti Tanti_params }
 "$"         / { allowAnti } { lexAnti Tanti_exp }
}

<0, qq> {
 ^ "#line" $whitechar+ $digit+ $whitechar+ \" [^\"]* \" .* { setLineFromPragma }
 ^ "#" $whitechar+ $digit+ $whitechar+ \" [^\"]* \" .*     { setLineFromPragma }

 "//" .* ;
 "/*" ([^\*]|[\r\n]|("*"+([^\*\/]|[\r\n])))* "*"+ "/" ;

 ^ "#" .*        ;
 $whitechar+     ;
 "__extension__" ;

 $nondigit ($nondigit | $digit)* { identifier }

 @floatingConstant                    { lexFloat }
 @decimalConstant @integerSuffix?     { lexInteger 0 decimal }
 @octalConstant @integerSuffix?       { lexInteger 1 octal }
 @hexadecimalConstant @integerSuffix? { lexInteger 2 hexadecimal }

 \' { lexCharTok }
 \" { lexStringTok }

 "("   { token Tlparen }
 ")"   { token Trparen }
 "["   { token Tlbrack }
 "]"   { token Trbrack }
 "{"   { token Tlbrace }
 "}"   { token Trbrace }
 ","   { token Tcomma }
 ";"   { token Tsemi }
 ":"   { token Tcolon }
 "?"   { token Tquestion }
 "."   { token Tdot }
 "->"  { token Tarrow }
 "..." { token Tellipses }

 "+"  { token Tplus }
 "-"  { token Tminus }
 "*"  { token Tstar }
 "/"  { token Tdiv }
 "%"  { token Tmod }
 "~"  { token Tnot }
 "&"  { token Tand }
 "|"  { token Tor }
 "^"  { token Txor }
 "<<" { token Tlsh }
 ">>" { token Trsh }
 "++" { token Tinc }
 "--" { token Tdec }

 "!"  { token Tlnot }
 "&&" { token Tland }
 "||" { token Tlor }

 "==" { token Teq }
 "!=" { token Tne }
 "<"  { token Tlt }
 ">"  { token Tgt }
 "<=" { token Tle }
 ">=" { token Tge }

 "="   { token Tassign }
 "+="  { token Tadd_assign }
 "-="  { token Tsub_assign }
 "*="  { token Tmul_assign }
 "/="  { token Tdiv_assign }
 "%="  { token Tmod_assign }
 "&="  { token Tand_assign }
 "|="  { token Tor_assign }
 "^="  { token Txor_assign }
 "<<=" { token Tlsh_assign }
 ">>=" { token Trsh_assign }

 "<<<" / { ifExtension cudaExts }
         { token T3lt }

 ">>>" / { ifExtension cudaExts }
         { token T3gt }
}

{
charEscapes :: [(Char, Char)]
charEscapes = [('n', '\n'),
               ('t', '\t'),
               ('v', '\v'),
               ('b', '\b'),
               ('r', '\r'),
               ('f', '\f'),
               ('a', '\a'),
               ('\\', '\\'),
               ('?', '?'),
               ('\'', '\''),
               ('\"', '\"')
              ]

type Action = Pos -> B.ByteString -> Int -> P (L Token)

setLineFromPragma :: Action
setLineFromPragma pos buf len = do
    setPos pos'
    lexToken
  where
    (_ : l : ws) = (words . B.unpack . B.take len) buf
    line = read l - 1
    filename = (takeWhile (/= '\"') . drop 1 . concat . intersperse " ") ws

    pos' :: Pos
    pos' = Pos (intern filename) line 1 (posCoff pos)

locateTok :: Pos -> Token -> P (L Token)
locateTok end tok = do
    start <- getLastPos
    setLastPos end
    return $ L (Loc start end) tok

token :: Token -> Action
token tok pos buf len =
    locateTok pos tok

identifier :: Action
identifier pos buf len =
    case Map.lookup kw keywordMap of
      Nothing ->              nonKeyword
      Just (tok, Nothing) ->  keyword tok
      Just (tok, Just i) ->   do  isKw <- useExts i
                                  if isKw then keyword tok else nonKeyword
  where
    kw = (B.unpack . B.take len) buf

    keyword tok  = locateTok pos tok

    nonKeyword = do
        test <- isTypedef kw
        if test
          then locateTok pos (Tnamed kw)
          else locateTok pos (Tidentifier kw)

lexAnti ::(String -> Token) ->  Action
lexAnti antiTok pos buf len = do
    c <- alexGetCharOrFail
    s <- case c of
           '('                 -> lexExpression 0 ""
           _ | isIdStartChar c -> lexIdChars [c]
             | otherwise       -> illegalCharacterLiteral pos
    end <- getPos
    locateTok end (antiTok s)
  where
    lexIdChars :: String -> P String
    lexIdChars s = do
        inp     <- getInput
        maybe_c <- alexMaybeGetChar
        case maybe_c of
          Nothing ->             return (reverse s)
          Just c | isIdChar c -> lexIdChars (c : s)
                 | otherwise ->  setInput inp >> return (reverse s)

    lexExpression :: Int -> String -> P String
    lexExpression depth s = do
        inp     <- getInput
        maybe_c <- alexMaybeGetChar
        case maybe_c of
          Nothing ->               return (reverse s)
          Just '(' ->              lexExpression (depth+1) ('(' : s)
          Just ')' | depth == 0 -> return (reverse s)
                   | otherwise ->  lexExpression (depth-1) (')' : s)
          Just c ->                lexExpression depth (c : s)

    isIdStartChar :: Char -> Bool
    isIdStartChar '_' = True
    isIdStartChar c   = isLower c

    isIdChar :: Char -> Bool
    isIdChar '_'  = True
    isIdChar '\'' = True
    isIdChar c    = isAlphaNum c

lexCharTok :: Action
lexCharTok pos buf len = do
    c <- alexGetCharOrFail
    case c of
      '\''  -> emptyCharacterLiteral pos
      '\\'  -> do  c <- lexEscape
                   quote <- alexGetCharOrFail
                   when (quote /= '\'') $
                       illegalCharacterLiteral pos
                   end <- getPos
                   raw <- liftM ('\'' :) (getBuffRange pos end)
                   locateTok end (TcharConst (raw, c))
      _ ->     do  quote <- alexGetCharOrFail
                   when (quote /= '\'') $
                       illegalCharacterLiteral pos
                   end <- getPos
                   raw <- liftM ('\'' :) (getBuffRange pos end)
                   locateTok end (TcharConst (raw, c))

lexStringTok :: Action
lexStringTok pos buf len = do
    s    <- lexString ""
    end  <- getPos
    raw  <- liftM ('"' :) (getBuffRange pos end)
    locateTok end (TstringConst (raw, s))
  where
    lexString :: String -> P String
    lexString s = do
        c <- alexGetCharOrFail
        case c of
          '"'   -> return (reverse s)
          '\\'  -> do  c' <- lexEscape
                       lexString (c' : s)
          _     -> lexString (c : s)

lexEscape :: P Char
lexEscape = do
    inp <- getInput
    c <- alexGetCharOrFail
    case c of
      'x' -> do  i <- checkedReadNum isHexDigit 16 hexDigit
                 return $ chr i
      n | isDigit n ->
              do  setInput inp
                  i <- checkedReadNum isOctDigit 8 octDigit
                  return $ chr i
      c -> case lookup c charEscapes of
             Nothing -> return c
             Just c' -> return c'

type Radix = (Integer, Char -> Int)

decDigit :: Char -> Int
decDigit c  | c >= '0' && c <= '9' = ord c - ord '0'
            | otherwise            = error "error in decimal constant"

octDigit :: Char -> Int
octDigit c  | c >= '0' && c <= '7' = ord c - ord '0'
            | otherwise            = error "error in octal constant"

hexDigit :: Char -> Int
hexDigit c  | c >= 'a' && c <= 'f' = ord c - ord 'a'
            | c >= 'A' && c <= 'F' = ord c - ord 'A'
            | c >= '0' && c <= '9' = ord c - ord '0'
            | otherwise            = error "error in hexadecimal constant"

decimal :: Radix
decimal = (10, decDigit)

octal :: Radix
octal = (8, octDigit)

hexadecimal :: Radix
hexadecimal = (16, hexDigit)

readInteger :: Radix -> ReadS Integer
readInteger (radix, charToInt) =
    go 0
  where
    go :: Integer -> ReadS Integer
    go  x  []                     = return (x, "")
    go  x  (c : cs)  | isDigit c  = go (x * radix + toInteger (charToInt c)) cs
                     | otherwise  = return (x, c : cs)

readRational :: ReadS Rational
readRational s = do
    (n, d, t)  <- readFix s
    (x, u)     <- readExponent t
    return ((n % 1) * 10^^(x - toInteger d), t)
  where
    readFix :: String ->  [(Integer, Int, String)]
    readFix s =
        return (read (i ++ f), length f, u)
      where
        (i, t) = span isDigit s
        (f, u) = case t of
                   '.' : u ->  span isDigit u
                   _ ->        ("", t)

    readExponent :: ReadS Integer
    readExponent ""                        = return (0, "")
    readExponent (e : s)  | e `elem` "eE"  = go s
                          | otherwise      = return (0, s)
      where
        go :: ReadS Integer
        go  ('+' : s)  = readDecimal s
        go  ('-' : s)  = do  (x, t) <- readDecimal s
                             return (-x, t)
        go  s          = readDecimal s

    readDecimal :: ReadS Integer
    readDecimal = readInteger decimal

lexInteger  ::  Int
            ->  Radix
            ->  Action
lexInteger ndrop radix pos buf len =
    case i of
      [n] ->  locateTok pos (toToken n)
      _ ->    fail "bad parse for integer"
  where
    num :: String
    num = (takeWhile isDigit . drop ndrop)  s

    suffix :: String
    suffix = (map toLower . takeWhile (not . isDigit) . reverse) s

    s :: String
    s = (B.unpack . B.take len) buf

    i :: [Integer]
    i = do  (n, _) <- readInteger radix s
            return n

    toToken :: Integer -> Token
    toToken n =
        case numElls of
          0 -> TintConst (s, isUnsigned, n)
          1 -> TlongIntConst (s, isUnsigned, n)
          2 -> TlongLongIntConst (s, isUnsigned, n)
      where
        numElls :: Int
        numElls = (length . filter (== 'l')) suffix

        isUnsigned :: C.Signed
        isUnsigned = if 'u' `elem` suffix then C.Unsigned else C.Signed

lexFloat :: Action
lexFloat pos buf len =
    case i of
      [n] ->  locateTok pos (toToken n)
      _ ->    fail "bad parse for integer"
  where
    s :: String
    s = (B.unpack . B.take len) buf

    prefix :: String
    prefix = takeWhile (not . isSuffix) s

    suffix :: String
    suffix = (map toLower . takeWhile isSuffix . reverse) s

    isSuffix :: Char -> Bool
    isSuffix = (`elem` ['l', 'L', 'f', 'F'])

    i :: [Rational]
    i = do  (n, _) <- readRational s
            return n

    toToken :: Rational -> Token
    toToken n =
        case suffix of
          "" ->   TdoubleConst (s, n)
          "f" ->  TfloatConst (s, n)
          "l" ->  TlongDoubleConst (s, n)

checkedReadNum :: (Char -> Bool) -> Int -> (Char -> Int) -> P Int
checkedReadNum isDigit base conv = do
    c <- alexGetCharOrFail
    when (not $ isDigit c) $
        getPos >>= illegalNumericalLiteral
    readNum isDigit base conv
  where
    readNum :: (Char -> Bool) -> Int -> (Char -> Int) -> P Int
    readNum isDigit base conv = read 0
      where
          read :: Int -> P Int
          read i = do
              inp <- getInput
              c <- alexGetCharOrFail
              if isDigit c
                then read (i*base + conv c)
                else setInput inp >> return i

lexToken :: P (L Token)
lexToken = do
  inp@(AlexInput pos buf off) <- getInput
  sc <- getLexState
  st <- get
  case alexScanUser st inp sc of
    AlexEOF -> return $ L (Loc pos pos) Teof
    AlexError (AlexInput pos2 _ _) ->
        lexerError pos2 ((text . B.unpack . B.take (min 80 (B.length rest))) rest)
      where
        rest = B.drop off buf
    AlexSkip inp2 _ -> do
        setInput inp2
        pos <- getPos
        setLastPos pos
        lexToken
    AlexToken inp2@(AlexInput end _ _) len t -> do
        setInput inp2
        t end (B.drop off buf) len

emptyCharacterLiteral :: Pos -> P a
emptyCharacterLiteral pos =
    throw $ ParserException (getLoc pos) (text "empty character literal")

illegalCharacterLiteral :: Pos -> P a
illegalCharacterLiteral pos =
    throw $ ParserException (getLoc pos) (text "illegal character literal")

illegalNumericalLiteral :: Pos -> P a
illegalNumericalLiteral pos =
    throw $ ParserException (getLoc pos) (text "illegal numerical literal")

lexerError :: Pos -> Doc -> P a
lexerError pos s =
    throw $ ParserException (getLoc pos) (text "lexer error on" <+> squotes s)
}
