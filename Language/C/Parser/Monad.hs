-- Copyright (c) 2006-2011
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
--
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
-- Module      :  Language.C.Parser.Monad
-- Copyright   :  (c) Harvard University 2006-2011
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.C.Parser.Monad (
    P,
    runP,
    evalP,

    PState,
    ParseContext(..),
    emptyPState,

    getInput,
    setInput,
    pushLexState,
    popLexState,
    getLexState,
    getCurToken,
    setCurToken,
    getParseContext,
    setParseContext,

    addTypedef,
    addVariable,
    isTypedef,

    pushScope,
    popScope,

    gccExts,
    cudaExts,
    openCLExts,

    useExts,
    useGccExts,
    useCUDAExts,
    useOpenCLExts,

    LexerException(..),
    ParserException(..),
    failAt,
    lexerError,
    unexpectedEOF,
    emptyCharacterLiteral,
    illegalCharacterLiteral,
    illegalNumericalLiteral,
    parserError,
    unclosed,
    expected,

    AlexInput(..),
    alexGetChar,
    alexGetByte,
    alexInputPrevChar,
    nextChar,
    peekChar,
    maybePeekChar,
    skipChar,

    AlexPredicate,
    allowAnti,
    ifExtension
  ) where

import Control.Applicative (Applicative(..))
import Control.Monad.Exception
import Control.Monad.Identity
import Control.Monad.State
import Data.Bits
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Internal (c2w)
import Data.List (foldl')
import Data.Loc
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import Data.Word
import Text.PrettyPrint.Mainland

import Language.C.Parser.Tokens
import Language.C.Syntax

data ParseContext  =  ParseDirect
                   |  ParseQuasiQuote

data PState = PState
    { inp        :: !AlexInput
    , curToken   :: L Token
    , lexState   :: ![Int]
    , context    :: !ParseContext
    , extensions :: !ExtensionsInt
    , typedefs   :: !(Set.Set String)
    , scopes     :: [Set.Set String]
    }

emptyPState :: [Extensions]
            -> [String]
            -> ParseContext
            -> B.ByteString
            -> Pos
            -> PState
emptyPState exts typnames ctx buf pos = PState
    { inp         = inp
    , curToken    = error "no token"
    , lexState    = [sc]
    , context     = ctx
    , extensions  = foldl' setBit 0 (map fromEnum exts)
    , typedefs    = Set.fromList typnames
    , scopes      = []
    }
  where
    sc :: Int
    sc = case ctx of
           ParseDirect     -> 0
           ParseQuasiQuote -> 1 {- qq lexer state -}

    inp :: AlexInput
    inp = AlexInput
          { alexPos      = pos
          , alexPrevChar = '\n'
          , alexInp      = buf
          , alexOff      = 0
          }

newtype P a = P { runP :: PState -> Either SomeException (a, PState) }

instance Functor P where
    fmap f x = x >>= return . f

instance Applicative P where
    pure  = return
    (<*>) = ap

instance Monad P where
    m >>= k = P $ \s ->
        case runP m s of
          Left e         -> Left e
          Right (a, s')  -> runP (k a) s'

    m1 >> m2 = P $ \s ->
        case runP m1 s of
          Left e         -> Left e
          Right (_, s')  -> runP m2 s'

    return a = P $ \s -> Right (a, s)

    fail msg = do  inp <- getInput
                   throw (ParserException (Loc (alexPos inp) (alexPos inp)) (ppr (alexPos inp)))

instance MonadState PState P where
    get    = P $ \s -> Right (s, s)
    put s  = P $ \_ -> Right ((), s)

instance MonadException P where
    throw e = P $ \_ -> Left (toException e)

    m `catch` h = P $ \s ->
        case runP m s of
          Left e ->
              case fromException e of
                Just e'  -> runP (h e') s
                Nothing  -> Left e
          Right (a, s')  -> Right (a, s')

evalP :: P a -> PState -> Either SomeException a
evalP comp st =
    case runP comp st of
      Left e        -> Left e
      Right (a, _)  -> Right a

getInput  :: P AlexInput
getInput = gets inp

setInput  :: AlexInput -> P ()
setInput inp= modify $ \s ->
    s { inp = inp }

pushLexState :: Int -> P ()
pushLexState ls = modify $ \s ->
    s { lexState = ls : lexState s }

popLexState :: P Int
popLexState = do
    ls <- getLexState
    modify $ \s ->
        s { lexState = tail (lexState s) }
    return ls

getLexState :: P Int
getLexState = gets (head . lexState)

getCurToken :: P (L Token)
getCurToken = gets curToken

setCurToken :: L Token -> P ()
setCurToken tok = modify $ \s -> s { curToken = tok }

getParseContext :: P ParseContext
getParseContext = gets context

setParseContext :: ParseContext -> P ()
setParseContext ctx = modify $ \s ->
    s { context = ctx }

addTypedef :: String -> P ()
addTypedef id = modify $ \s ->
    s { typedefs = Set.insert id (typedefs s) }

addVariable :: String -> P ()
addVariable id = modify $ \s ->
    s { typedefs = Set.delete id (typedefs s) }

isTypedef :: String -> P Bool
isTypedef id = gets $ \s ->
    Set.member id (typedefs s)

pushScope :: P ()
pushScope = modify  $ \s ->
    s { scopes = typedefs s : scopes s }

popScope :: P ()
popScope = modify  $ \s ->
    s { scopes     = (tail . scopes) s
      , typedefs   = (head . scopes) s
      }

gccExts :: ExtensionsInt
gccExts = (bit . fromEnum) Gcc

cudaExts :: ExtensionsInt
cudaExts = (bit . fromEnum) CUDA

openCLExts :: ExtensionsInt
openCLExts = (bit . fromEnum) OpenCL

useExts :: ExtensionsInt -> P Bool
useExts ext = gets $ \s ->
    extensions s .&. ext /= 0

useGccExts :: P Bool
useGccExts = useExts gccExts

useCUDAExts :: P Bool
useCUDAExts = useExts cudaExts

useOpenCLExts :: P Bool
useOpenCLExts = useExts openCLExts

data LexerException = LexerException Pos Doc
  deriving (Typeable)

instance Exception LexerException where

instance Show LexerException where
    show (LexerException pos msg) =
        show $ nest 4 $ ppr pos <> text ":" </> msg

data ParserException = ParserException Loc Doc
  deriving (Typeable)

instance Exception ParserException where

instance Show ParserException where
    show (ParserException loc msg) =
        show $ nest 4 $ ppr loc <> text ":" </> msg

failAt :: Loc -> String -> P a
failAt loc msg =
    throw $ ParserException loc (text msg)

lexerError :: AlexInput -> Doc -> P a
lexerError inp s =
    throw $ LexerException (alexPos inp) (text "lexer error on" <+> squotes s)

unexpectedEOF :: AlexInput -> P a
unexpectedEOF inp =
    lexerError inp  (text "unexpected end of file")

emptyCharacterLiteral :: AlexInput -> P a
emptyCharacterLiteral inp =
    lexerError inp (text "empty character literal")

illegalCharacterLiteral :: AlexInput -> P a
illegalCharacterLiteral inp =
    lexerError inp (text "illegal character literal")

illegalNumericalLiteral :: AlexInput -> P a
illegalNumericalLiteral inp =
    lexerError inp (text "illegal numerical literal")

parserError :: Loc -> Doc -> P a
parserError loc msg =
    throw $ ParserException loc msg

unclosed :: Loc -> String -> P a
unclosed loc x =
    parserError (locEnd loc) (text "unclosed" <+> squotes (text x))

expected :: [String] -> P b
expected alts = do
    tok@(L loc _) <- getCurToken
    parserError (locStart loc) (text "expected" <+> pprAlts alts <+> pprGot tok)
  where
    pprAlts :: [String] -> Doc
    pprAlts []        = empty
    pprAlts [s]       = text s
    pprAlts [s1, s2]  = text s1 <+> text "or" <+> text s2
    pprAlts (s : ss)  = text s <> comma <+> pprAlts ss

    pprGot :: L Token -> Doc
    pprGot (L _ Teof)  = text "but reached end of file"
    pprGot (L _ t)     = text "but got" <+> text (show t)

data AlexInput = AlexInput
  {  alexPos      :: {-#UNPACK#-} !Pos
  ,  alexPrevChar :: {-#UNPACK#-} !Char
  ,  alexInp      :: {-#UNPACK#-} !B.ByteString
  ,  alexOff      :: {-#UNPACK#-} !Int
  }

alexGetChar :: AlexInput -> Maybe (Char, AlexInput)
alexGetChar inp =
    case B.uncons (alexInp inp) of
      Nothing       -> Nothing
      Just (c, bs)  -> Just (c, inp  {  alexPos       = advancePos (alexPos inp) c
                                     ,  alexPrevChar  = c
                                     ,  alexInp       = bs
                                     ,  alexOff       = alexOff inp + 1
                                     })

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte inp =
    case alexGetChar inp of
      Nothing        -> Nothing
      Just (c, inp') -> Just (c2w c, inp')

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = alexPrevChar

nextChar :: P Char
nextChar = do
    inp <- getInput
    case alexGetChar inp of
      Nothing         -> unexpectedEOF inp
      Just (c, inp')  -> setInput inp' >> return c

peekChar ::P Char
peekChar = do
    inp <- getInput
    case B.uncons (alexInp inp) of
      Nothing      -> unexpectedEOF inp
      Just (c, _)  -> return c

maybePeekChar :: P (Maybe Char)
maybePeekChar = do
    inp <- getInput
    case alexGetChar inp of
      Nothing      -> return Nothing
      Just (c, _)  -> return (Just c)

skipChar :: P ()
skipChar = do
    inp <- getInput
    case alexGetChar inp of
      Nothing         -> unexpectedEOF inp
      Just (_, inp')  -> setInput inp'

-- | The components of an 'AlexPredicate' are the predicate state, input stream
-- before the token, length of the token, input stream after the token.
type AlexPredicate =  PState
                   -> AlexInput
                   -> Int
                   -> AlexInput
                   -> Bool

allowAnti :: AlexPredicate
allowAnti  (PState { context = ParseQuasiQuote })  _ _ _  = True
allowAnti  _                                       _ _ _  = False

ifExtension :: ExtensionsInt -> AlexPredicate
ifExtension i s _ _ _ = extensions s .&. i /= 0
