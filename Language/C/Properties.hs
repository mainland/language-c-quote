-- |
-- Module      :  Language.C.Properties
-- Copyright   :  (c) 2006-2008 Harvard University
--             :  (c) 2013-2015 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Language.C.Properties where

import qualified Data.ByteString.Char8 as B
import Data.Loc
import Text.PrettyPrint.Mainland

import Language.C.Syntax as C
import qualified Language.C.Parser as P

prop_ParsePrintUnitId :: [C.Extensions] -> B.ByteString -> Bool
prop_ParsePrintUnitId exts s_ =
    case comp s_ of
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
        case P.parse exts [] P.parseUnit s (Just pos) of
          Left err   -> fail $ show err
          Right defs -> return defs
      where
        pos = startPos "<internal>"
