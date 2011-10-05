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
-- Copyright   :  (c) Harvard University 2006-2010
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu
--
--------------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.C.Parser.Monad (
    AlexInput(..),
    alexGetChar,
    alexMaybeGetChar,
    alexGetCharOrFail,
    alexInputPrevChar,
    allowAnti,
    ifExtension,

    ParserException(..),
    ParseContext(..),
    PState,
    emptyPState,

    P,
    runP,
    evalP,

    failAt,

    getBuffer,
    setBuffer,
    getBuffRange,
    getLastPos,
    setLastPos,
    getPos,
    setPos,
    pushLexState,
    popLexState,
    getLexState,
    getParseContext,
    setParseContext,
    getInput,
    setInput,

    addTypedef,
    addVariable,
    isTypedef,

    pushScope,
    popScope,

    gccExts,
    cudaExts,

    useExts,
    useGccExts,
    useCUDAExts
  ) where

import Control.Monad.Exception
import Control.Monad.Identity
import Control.Monad.State
import Data.Bits
import qualified Data.ByteString.Char8 as B
import Data.List (foldl')
import Data.Loc
import qualified Data.Set as Set
import Data.Typeable (Typeable)
import Text.PrettyPrint.Mainland

import Language.C.Parser.Tokens
import Language.C.Syntax

data AlexInput = AlexInput {-#UNPACK#-} !Pos
                           {-#UNPACK#-} !B.ByteString
                           {-#UNPACK#-} !Int

alexGetChar :: AlexInput -> Maybe (Char, AlexInput)
alexGetChar (AlexInput pos buf off)
    | off < B.length buf  =  c `seq` pos' `seq` off' `seq`
                             Just (c, AlexInput pos' buf off')
    | otherwise           =  Nothing
  where
    c     = B.index buf off
    pos'  = advancePos pos c
    off'  = off + 1

alexMaybeGetChar :: P (Maybe Char)
alexMaybeGetChar = do
    inp <- getInput
    case alexGetChar inp of
      Nothing -> return Nothing
      Just (c, inp')  -> setInput inp' >> return (Just c)

alexGetCharOrFail :: P Char
alexGetCharOrFail = do
    inp <- getInput
    case alexGetChar inp of
      Nothing         -> fail "unexpected end of file"
      Just (c, inp')  -> setInput inp' >> return c

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (AlexInput  _  _    0)    = '\n'
alexInputPrevChar (AlexInput  _  buf  off)  = B.index buf (off - 1)

-- | The components of an 'AlexPredicate' are the predicate state, input stream
-- before the token, length of the token, input stream after the token.
type AlexPredicate  =   PState
                    ->  AlexInput
                    ->  Int
                    ->  AlexInput
                    ->  Bool

allowAnti :: AlexPredicate
allowAnti  (PState { context = ParseQuasiQuote })  _ _ _  = True
allowAnti  _                                       _ _ _  = False

ifExtension :: ExtensionsInt -> AlexPredicate
ifExtension i s _ _ _ = extensions s .&. i /= 0

data ParserException = ParserException Loc Doc
  deriving (Typeable)

instance Exception ParserException where

instance Show ParserException where
    show (ParserException loc msg) =
        show $ nest 4 $ ppr loc <> text ":" </> msg

data ParseContext  =  ParseDirect
                   |  ParseQuasiQuote

data PState = PState
    { buf         :: !B.ByteString  -- ^ Buffer we're parsing
    , off         :: !Int           -- ^ Current offset in the buffer
    , lastPos     :: !Pos           -- ^ End position of last token parsed
    , pos         :: !Pos           -- ^ Current source code position
    , lexState    :: ![Int]
    , context     :: !ParseContext
    , extensions  :: !ExtensionsInt

    , typedefs    :: !(Set.Set String)
    , scopes      :: [Set.Set String]
    }

emptyPState :: [Extensions]
            -> [String]
            -> ParseContext
            -> B.ByteString
            -> Pos
            -> PState
emptyPState exts typnames ctx bs pos = PState
    { buf         = bs
    , off         = 0
    , lastPos     = pos
    , pos         = pos
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

newtype P a = P { unP :: StateT PState (ExceptionT Identity) a }
  deriving (MonadException,
            MonadState PState)

instance Monad P where
    m >>= f   = P $ unP m >>= unP . f
    m1 >> m2  = P $ unP m1 >> unP m2
    return    = P . return
    fail msg  = do  pos <- getPos
                    throw $ ParserException (getLoc pos) (text msg)

runP :: P a -> PState -> Either SomeException (a, PState)
runP m = runIdentity . runExceptionT . runStateT (unP m)

evalP :: P a -> PState -> Either SomeException a
evalP m = runIdentity . runExceptionT . evalStateT (unP m)

failAt :: Loc -> String -> P a
failAt loc msg =
    throw $ ParserException loc (text msg)

getBuffer :: P B.ByteString
getBuffer = gets buf

setBuffer  :: B.ByteString -> P ()
setBuffer buf = modify $ \s ->
    s { buf = buf }

getBuffRange :: Pos -> Pos -> P String
getBuffRange (Pos _ _ _ start) (Pos _ _ _ end) = do
    b <- gets buf
    return $ (B.unpack . B.take (end - start) . B.drop start) b

getLastPos  :: P Pos
getLastPos = gets lastPos

setLastPos  :: Pos -> P ()
setLastPos pos = modify $ \s ->
    s { lastPos = pos }

getPos :: P Pos
getPos = gets pos

setPos :: Pos -> P ()
setPos pos = modify $ \s ->
    s { pos = pos }

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

getParseContext :: P ParseContext
getParseContext = gets context

setParseContext :: ParseContext -> P ()
setParseContext ctx = modify $ \s ->
    s { context = ctx }

getInput  :: P AlexInput
getInput = gets $ \s ->
    AlexInput (pos s) (buf s) (off s)

setInput  :: AlexInput -> P ()
setInput (AlexInput p b o) = modify $ \s ->
    s { buf = b, off = o, pos = p }

addTypedef :: String -> P ()
addTypedef id = modify  $ \s ->
    s { typedefs = Set.insert id (typedefs s) }

addVariable :: String -> P ()
addVariable id = modify  $ \s ->
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

useExts :: ExtensionsInt -> P Bool
useExts ext = gets $ \s ->
    extensions s .&. ext /= 0

gccExts :: ExtensionsInt
gccExts = (bit . fromEnum) Gcc

cudaExts :: ExtensionsInt
cudaExts = (bit . fromEnum) CUDA

useGccExts :: P Bool
useGccExts = useExts gccExts

useCUDAExts :: P Bool
useCUDAExts = useExts cudaExts
