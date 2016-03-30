-- |
-- Module      :  Language.C.Parser
-- Copyright   :  (c)  2006-2010 Harvard University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Language.C.Parser (
    module Language.C.Parser.Lexer,
    module Language.C.Parser.Monad,
    module Language.C.Parser.Parser,
    parse
  ) where

import Control.Exception

import qualified Data.ByteString.Char8 as B
import Data.Loc

import Language.C.Parser.Lexer
import Language.C.Parser.Parser
import Language.C.Parser.Monad
import Language.C.Syntax

parse :: [Extensions]
      -> [String]
      -> P a
      -> B.ByteString
      -> Maybe Pos
      -> Either SomeException a
parse exts typnames p bs pos =
    evalP p (emptyPState exts typnames bs pos)
