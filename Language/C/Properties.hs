-- |
-- Module      :  Language.C.Properties
-- Copyright   :  (c) Harvard University 2006-2008
--             :  (c) Drexel University 2013
-- License     :  BSD-style
-- Maintainer  :  mainland@cs.drexel.edu

module Language.C.Properties where

import qualified Data.ByteString.Char8 as B
import Data.Loc
import Data.Symbol
import Text.PrettyPrint.Mainland

import Language.C.Syntax as C
import qualified Language.C.Parser as P

prop_ParsePrintUnitId :: [C.Extensions] -> B.ByteString -> Bool
prop_ParsePrintUnitId exts s =
    case comp s of
      Left _ ->  False
      Right x -> x
  where
    comp :: B.ByteString -> Either String Bool
    comp s = do
        defs   <- parse s
        defs'  <- parse ((B.pack . pretty 80 . ppr) defs)
        return $ defs' == defs

    parse :: B.ByteString -> Either String [C.Definition]
    parse s =
        case P.parse exts [] P.parseUnit s pos of
          Left err   -> fail $ show err
          Right defs -> return defs
      where
        pos = startPos "<internal>"
