-- -*- mode: literate-haskell -*-

{
{-# OPTIONS -w #-}
{-# LANGUAGE CPP #-}

-- |
-- Module      :  Language.C.Parser.Lexer
-- Copyright   :  (c) 2006-2011 Harvard University
--                (c) 2011-2013 Geoffrey Mainland
--                (c) 2013-2015 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Language.C.Parser.Lexer (
    lexToken
  ) where

import Control.Applicative
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
                  ord,
                  chr,
                  toLower)
import Data.List (foldl',
                  intersperse,
                  isPrefixOf)
import Data.Loc
import qualified Data.Map as Map
import Data.Ratio ((%))
import qualified Data.Set as Set
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

@ccomment   = "//" .*
@cppcomment = "/*" ([^\*]|[\r\n]|("*"+([^\*\/]|[\r\n])))* "*"+ "/"

c :-

<0> {
 "typename"  / { allowAnti } { token Ttypename }

 "$id:"      / { allowAnti } { lexAnti Tanti_id }
 "$const:"   / { allowAnti } { lexAnti Tanti_const }
 "$int:"     / { allowAnti } { lexAnti Tanti_int }
 "$uint:"    / { allowAnti } { lexAnti Tanti_uint }
 "$lint:"    / { allowAnti } { lexAnti Tanti_lint }
 "$ulint:"   / { allowAnti } { lexAnti Tanti_ulint }
 "$llint:"   / { allowAnti } { lexAnti Tanti_llint }
 "$ullint:"  / { allowAnti } { lexAnti Tanti_ullint }
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
 "$escstm:"  / { allowAnti } { lexAnti Tanti_escstm }
 "$edecl:"   / { allowAnti } { lexAnti Tanti_edecl }
 "$edecls:"  / { allowAnti } { lexAnti Tanti_edecls }
 "$item:"    / { allowAnti } { lexAnti Tanti_item }
 "$items:"   / { allowAnti } { lexAnti Tanti_items }
 "$stm:"     / { allowAnti } { lexAnti Tanti_stm }
 "$stms:"    / { allowAnti } { lexAnti Tanti_stms }
 "$tyqual:"  / { allowAnti } { lexAnti Tanti_type_qual }
 "$tyquals:" / { allowAnti } { lexAnti Tanti_type_quals }
 "$ty:"      / { allowAnti } { lexAnti Tanti_type }
 "$spec:"    / { allowAnti } { lexAnti Tanti_spec }
 "$param:"   / { allowAnti } { lexAnti Tanti_param }
 "$params:"  / { allowAnti } { lexAnti Tanti_params }
 "$pragma:"  / { allowAnti } { lexAnti Tanti_pragma }
 "$comment:" / { allowAnti } { lexAnti Tanti_comment }
 "$init:"    / { allowAnti } { lexAnti Tanti_init }
 "$inits:"   / { allowAnti } { lexAnti Tanti_inits }
 "$attr:"    / { allowAnti } { lexAnti Tanti_attr }
 "$attrs:"   / { allowAnti } { lexAnti Tanti_attrs }
 "$"         / { allowAnti } { lexAnti Tanti_exp }

 --
 -- Objective-C
 --
 "$ifdecl:"     / { allowAnti } { lexAnti Tanti_objc_ifdecl }
 "$ifdecls:"    / { allowAnti } { lexAnti Tanti_objc_ifdecls }
 "$prop:"       / { allowAnti } { lexAnti Tanti_objc_prop }
 "$props:"      / { allowAnti } { lexAnti Tanti_objc_props }
 "$propattr:"   / { allowAnti } { lexAnti Tanti_objc_prop_attr }
 "$propattrs:"  / { allowAnti } { lexAnti Tanti_objc_prop_attrs }
 "$dictelems:"  / { allowAnti } { lexAnti Tanti_objc_dicts }
 "$methparam:"  / { allowAnti } { lexAnti Tanti_objc_param }
 "$methparams:" / { allowAnti } { lexAnti Tanti_objc_params }
 "$methproto:"  / { allowAnti } { lexAnti Tanti_objc_method_proto }
 "$methdef:"    / { allowAnti } { lexAnti Tanti_objc_method_def }
 "$methdefs:"   / { allowAnti } { lexAnti Tanti_objc_method_defs }
 "$recv:"       / { allowAnti } { lexAnti Tanti_objc_recv }
 "$kwarg:"      / { allowAnti } { lexAnti Tanti_objc_arg }
 "$kwargs:"     / { allowAnti } { lexAnti Tanti_objc_args }
}

<0> {
 ^ $whitechar* "#line" $whitechar+ $digit+ $whitechar+ \" [^\"]* \" .* { setLineFromPragma }
 ^ $whitechar* "#" $whitechar+ $digit+ $whitechar+ \" [^\"]* \" .*     { setLineFromPragma }

 $whitechar* "#" $whitechar* "pragma" $whitechar+ .* { pragmaTok }

 @ccomment ;
 @cppcomment ;

 ^ $whitechar* "#" .* ;
 $whitechar+          ;
 "__extension__"      ;

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

 "{" $whitechar* @ccomment   { commentTok Tlbrace }
 "{" $whitechar* @cppcomment { commentTok Tlbrace }

 ";" $whitechar* @ccomment   { commentTok Tsemi }
 ";" $whitechar* @cppcomment { commentTok Tsemi }

 --
 -- Objective-C
 --
 "@" / { ifExtension objcExts }
       { token TObjCat }

 --
 -- CUDA
 --
 "mutable" { token TCUDAmutable }

 "<<<" / { ifExtension cudaExts }
         { token TCUDA3lt }

 ">>>" / { ifExtension cudaExts }
         { token TCUDA3gt }
}

{
type Action = AlexInput -> AlexInput -> P (L Token)

inputString :: AlexInput -> AlexInput -> String
inputString beg end =
  (B.unpack . B.take (alexOff end - alexOff beg)) (alexInput beg)

locateTok :: AlexInput -> AlexInput -> Token -> L Token
locateTok beg end tok =
    L (alexLoc beg end) tok

token :: Token -> Action
token tok beg end =
    return $ locateTok beg end tok

setLineFromPragma :: Action
setLineFromPragma beg end = do
    inp <- getInput
    setInput inp { alexPos = pos' }
    lexToken
  where
    (_ : l : ws) = words (inputString beg end)
    line = read l - 1
    filename = (takeWhile (/= '\"') . drop 1 . concat . intersperse " ") ws

    pos' :: Maybe Pos
    pos' = case alexPos beg of
             Nothing  -> Nothing
             Just pos -> Just $ Pos filename line 1 (posCoff pos)

identifier :: Action
identifier beg end =
    case Map.lookup ident keywordMap of
      Nothing             -> nonKeyword
      Just (tok, Nothing) -> keyword tok
      Just (tok, Just i)  -> do  isKw <- useExts i
                                 if isKw then keyword tok else nonKeyword
  where
    ident :: String
    ident = inputString beg end

      -- NB: Due to the format of the keyword table, the lexer can't currently produce different
      --     keyword tokens for the same lexeme in dependence on the active language extension.
      --     We need to distinguish between the 'private' keyword of OpenCL and Objective-C, though,
      --     to avoid a large number of shift-reduce conflicts. Hence, the ugly special case below.
    keyword :: Token -> P (L Token)
    keyword TCLprivate =
        do isObjC <- useExts objcExts
           if isObjC
             then
               return $ locateTok beg end TObjCprivate
             else
               return $ locateTok beg end TCLprivate
    keyword tok =
        return $ locateTok beg end tok

    nonKeyword :: P (L Token)
    nonKeyword = do
        typeTest  <- isTypedef  ident
        classTest <- isClassdef ident
        return $
          if typeTest
          then locateTok beg end (Tnamed ident)
          else if classTest
          then locateTok beg end (TObjCnamed ident)
          else locateTok beg end (Tidentifier ident)

lexAnti ::(String -> Token) ->  Action
lexAnti antiTok beg end = do
    c <- nextChar
    s <- case c of
           '('                 -> lexExpression 0 ""
           _ | isIdStartChar c -> lexIdChars [c]
             | otherwise       -> lexerError beg (text "illegal antiquotation")
    return $ locateTok beg end (antiTok s)
  where
    lexIdChars :: String -> P String
    lexIdChars s = do
        maybe_c <- maybePeekChar
        case maybe_c of
          Just c | isIdChar c -> skipChar >> lexIdChars (c : s)
          _                   -> return (reverse s)

    lexExpression :: Int -> String -> P String
    lexExpression depth s = do
        maybe_c <- maybePeekChar
        case maybe_c of
          Nothing               -> do end <- getInput
                                      parserError (alexLoc beg end)
                                                  (text "unterminated antiquotation")
          Just '('              -> skipChar >> lexExpression (depth+1) ('(' : s)
          Just ')' | depth == 0 -> skipChar >> return (unescape (reverse s))
                   | otherwise  -> skipChar >> lexExpression (depth-1) (')' : s)
          Just c                -> skipChar >> lexExpression depth (c : s)
      where
        unescape :: String -> String
        unescape ('\\':'|':'\\':']':s)  = '|' : ']' : unescape s
        unescape (c:s)                  = c : unescape s
        unescape []                     = []

    isIdStartChar :: Char -> Bool
    isIdStartChar '_' = True
    isIdStartChar c   = isLower c

    isIdChar :: Char -> Bool
    isIdChar '_'  = True
    isIdChar '\'' = True
    isIdChar c    = isAlphaNum c

pragmaTok :: Action
pragmaTok beg end =
    return $ locateTok beg end (Tpragma (findPragma (inputString beg end)))
  where
    findPragma :: String -> String
    findPragma s | pragma `isPrefixOf` s =
        (trim . drop (length pragma)) s
      where
        trim = f . f
        f = reverse . dropWhile isSpace
    findPragma s =
        findPragma (tail s)

    pragma :: String
    pragma = "pragma"

-- XXX: Gross hack. We assume the first character of our input is the textual
-- representation of tok, e.g., '{' or ';'. We then scan to the first '/', which
-- we assume is the start of the comment.
commentTok :: Token -> Action
commentTok tok beg end = do
    pushbackToken $ locateTok commentBeg end (Tcomment (inputString commentBeg end))
    return $ locateTok beg tokEnd tok
  where
    tokEnd, commentBeg :: AlexInput
    tokEnd = case alexGetChar beg of
               Nothing          -> error "commentTok: the impossible happened"
               Just (_, tokEnd) -> tokEnd
    commentBeg  = findCommentStart tokEnd

    findCommentStart :: AlexInput -> AlexInput
    findCommentStart inp =
        case alexGetChar inp of
          Nothing          -> error "commentTok: the impossible happened"
          Just ('/', inp') -> inp
          Just (_,   inp') -> findCommentStart inp'

lexCharTok :: Action
lexCharTok beg cur = do
    c   <- nextChar >>= lexChar
    end <- getInput
    return $ locateTok beg end (TcharConst (inputString beg end, c))
  where
    lexChar :: Char -> P Char
    lexChar '\'' = emptyCharacterLiteral beg
    lexChar '\\' = do c <- lexCharEscape
                      assertNextChar '\''
                      return c
    lexChar c    = do assertNextChar '\''
                      return c

    assertNextChar :: Char -> P ()
    assertNextChar c = do
        c' <- nextChar
        when (c' /= c) $
            illegalCharacterLiteral cur

lexStringTok :: Action
lexStringTok beg _ = do
    s    <- lexString ""
    end  <- getInput
    return $ locateTok beg end (TstringConst (inputString beg end, s))
  where
    lexString :: String -> P String
    lexString s = do
        c <- nextChar
        case c of
          '"'  -> return (reverse s)
          '\\' -> do  c' <- lexCharEscape
                      lexString (c' : s)
          _    -> lexString (c : s)

lexCharEscape :: P Char
lexCharEscape = do
    cur  <- getInput
    c    <- nextChar
    case c of
      'a'  -> return '\a'
      'b'  -> return '\b'
      'f'  -> return '\f'
      'n'  -> return '\n'
      'r'  -> return '\r'
      't'  -> return '\t'
      'v'  -> return '\v'
      '\\' -> return '\\'
      '\'' -> return '\''
      '"'  -> return '"'
      '?'  -> return '?'
      'x'  -> chr <$> checkedReadNum isHexDigit 16 hexDigit
      n | isOctDigit n -> setInput cur >> chr <$> checkedReadNum isOctDigit 8 octDigit
      c -> return c

lexInteger :: Int -> Radix -> Action
lexInteger ndrop radix@(_, isRadixDigit, _) beg end =
    case i of
      [n] -> return $ locateTok beg end (toToken n)
      _   -> fail "bad parse for integer"
  where
    num :: String
    num = (takeWhile isRadixDigit . drop ndrop)  s

    suffix :: String
    suffix = (map toLower . takeWhile (not . isRadixDigit) . reverse) s

    s :: String
    s = inputString beg end

    i :: [Integer]
    i = do  (n, _) <- readInteger radix num
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
lexFloat beg end =
    case i of
      [n] -> return $ locateTok beg end (toToken n)
      _   -> fail "bad parse for float"
  where
    s :: String
    s = inputString beg end

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
          ""  -> TdoubleConst (s, fromRational n)
          "f" -> TfloatConst (s, fromRational n)
          "l" -> TlongDoubleConst (s, fromRational n)

type Radix = (Integer, Char -> Bool, Char -> Int)

decDigit :: Char -> Int
decDigit c  | c >= '0' && c <= '9' = ord c - ord '0'
            | otherwise            = error "error in decimal constant"

octDigit :: Char -> Int
octDigit c  | c >= '0' && c <= '7' = ord c - ord '0'
            | otherwise            = error "error in octal constant"

hexDigit :: Char -> Int
hexDigit c  | c >= 'a' && c <= 'f' = 10 + ord c - ord 'a'
            | c >= 'A' && c <= 'F' = 10 + ord c - ord 'A'
            | c >= '0' && c <= '9' = ord c - ord '0'
            | otherwise            = error "error in hexadecimal constant"

decimal :: Radix
decimal = (10, isDigit, decDigit)

octal :: Radix
octal = (8, isOctDigit, octDigit)

hexadecimal :: Radix
hexadecimal = (16, isHexDigit, hexDigit)

readInteger :: Radix -> ReadS Integer
readInteger (radix, isRadixDigit, charToInt) =
    go 0
  where
    go :: Integer -> ReadS Integer
    go  x  []             = return (x, "")
    go  x  (c : cs)
        | isRadixDigit c  = go (x * radix + toInteger (charToInt c)) cs
        | otherwise       = return (x, c : cs)

readDecimal :: ReadS Integer
readDecimal = readInteger decimal

readRational :: ReadS Rational
readRational s = do
    (n, d, t)  <- readFix s
    (x, _)     <- readExponent t
    return ((n % 1) * 10^^(x - toInteger d), t)
  where
    readFix :: String ->  [(Integer, Int, String)]
    readFix s =
        return (read (i ++ f), length f, u)
      where
        (i, t) = span isDigit s
        (f, u) = case t of
                   '.' : u  -> span isDigit u
                   _        -> ("", t)

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

checkedReadNum :: (Char -> Bool) -> Int -> (Char -> Int) -> P Int
checkedReadNum isDigit base conv = do
    cur  <- getInput
    c    <- peekChar
    when (not $ isDigit c) $
       illegalNumericalLiteral cur
    readNum isDigit base conv

readNum :: (Char -> Bool) -> Int -> (Char -> Int) -> P Int
readNum isDigit base conv =
    read 0
  where
    read :: Int -> P Int
    read n = do
        c <- peekChar
        if isDigit c
          then do  let n' = n*base + conv c
                   n' `seq` skipChar >> read n'
          else return n

lexToken :: P (L Token)
lexToken = do
    maybe_tok <- getPushbackToken
    case maybe_tok of
      Nothing  -> nextToken
      Just tok -> return tok
  where
    nextToken :: P (L Token)
    nextToken = do
        beg  <- getInput
        sc   <- getLexState
        st   <- get
        case alexScanUser st beg sc of
          AlexEOF ->
              return $ L (alexLoc beg beg) Teof
          AlexError end ->
              lexerError end (text rest)
            where
              rest :: String
              rest = B.unpack $ B.take 80 (alexInput end)
          AlexSkip end _ ->
              setInput end >> lexToken
          AlexToken end len t ->
              setInput end >> t beg end
}
