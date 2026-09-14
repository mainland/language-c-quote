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

import           Control.Exception

import qualified Data.ByteString.Char8    as B
import           Data.Loc

import           Language.C.Parser.Lexer
import           Language.C.Parser.Monad
import           Language.C.Parser.Parser
import           Language.C.Syntax

-- | Parse bytes, optionally tracking source positions. Each byte advances the
-- position by one character, with tabs and newlines updating the column and
-- line. With @srcloc >= 0.7@, line directives set the mapped filename and line
-- and make the character offset unknown. Without a starting position, parsed
-- nodes have no source location.
parse :: [Extensions]
      -> [String]
      -> P a
      -> B.ByteString
      -> Maybe Pos
      -> Either SomeException a
parse exts typnames p bs pos =
    evalP p (emptyPState exts typnames bs pos)
