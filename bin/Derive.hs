{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
-- Module      :  Derive
-- Copyright   :  (c) 2015 Drexel University
-- License     :  BSD-style
-- Maintainer  :  mainland@drexel.edu

module Derive (
    deriveM,
    deriveLocated,
    deriveRelocatable
  ) where

import Data.Generics
import Data.Loc
import Data.Symbol
import Text.PrettyPrint.Mainland as PP

deriveM :: (a -> Doc) -> a -> IO ()
deriveM derive (_ :: a) = do
    putDoc $ derive (undefined :: a)
    putStrLn ""

deriveLocated :: forall a . (Typeable a, Data a) => a -> Doc
deriveLocated _ =
    nest 2 $
    text "instance" <+>  text "Located" <+> text (tyConName typeName) <+> text "where" </>
    stack (map locDef constructors)
  where
    (typeName, _) = splitTyConApp (typeOf (undefined::a))

    constructors :: [(String, [String], Int)]
    constructors = map gen $ dataTypeConstrs (dataTypeOf (undefined::a))
      where
        gen :: Constr -> (String, [String], Int)
        gen con =
            ( showConstr con
            , gmapQ (showConstr . toConstr) (fromConstrB empty' con :: a)
            , gmapQl (+) 0 (const 1) (fromConstrB empty' con :: a)
            )

    locDef :: (String, [String], Int) -> Doc
    locDef (name, ks, ps) =
        nest 2 $
        text "locOf" <+> wrap pattern <+> text "=" <+/> text rhs
      where
        wrap | ps /= 0   = parens
             | otherwise = id

        (pats, rhs) = go ks
          where
            go :: [String] -> ([String], String)
            go []               = ([], "noLoc")
            go ("SrcLoc" : ks') = ("l" : replicate (length ks') "_", "locOf l")
            go (_ : ks')        = ("_" : pats', rhs')
              where
                (pats', rhs') = go ks'

        pattern = spread (text name : map text pats)

deriveRelocatable :: forall a . (Typeable a, Data a) => a -> Doc
deriveRelocatable _ =
    nest 2 $
    text "instance" <+>  text "Relocatable" <+> text (tyConName typeName) <+> text "where" </>
    stack (map locDef constructors)
  where
    (typeName, _) = splitTyConApp (typeOf (undefined::a))

    constructors :: [(String, [String], Int)]
    constructors = map gen $ dataTypeConstrs (dataTypeOf (undefined::a))
      where
        gen :: Constr -> (String, [String], Int)
        gen con =
            ( showConstr con
            , gmapQ (showConstr . toConstr) (fromConstrB empty' con :: a)
            , gmapQl (+) 0 (const 1) (fromConstrB empty' con :: a)
            )

    locDef :: (String, [String], Int) -> Doc
    locDef (name, ks, ps) =
        nest 2 $
        text "reloc" <+>
        (if usedloc then text "l" else text "_") <+>
        wrap (pattern lhspats) <+>
        text "=" <+/>
        wrap (pattern rhspats)
      where
        wrap | ps /= 0   = parens
             | otherwise = id

        (usedloc, lhspats, rhspats) = go 0 ks
          where
            go :: Int -> [String] -> (Bool, [String], [String])
            go _ [] = (False, [], [])

            go i ("SrcLoc" : ks') = (True, "_" : rest, "(fromLoc l)" : rest)
              where
                rest = ["x" ++ show j | j <- [i+1..length ks']]

            go i (_ : ks') = (usedloc', p : lhspats', p : rhspats')
              where
                p = "x" ++ show i
                (usedloc', lhspats', rhspats') = go (i+1) ks'

        pattern pats = spread (text name : map text pats)

empty' :: forall a. Data a => a
empty' = Data.Generics.empty
  `extB` pos
  `extB` loc
  `extB` sloc
  `extB` symbol
  where
    pos :: Pos
    pos = Pos "" 1 1 1

    loc :: Loc
    loc = NoLoc

    sloc :: SrcLoc
    sloc = SrcLoc NoLoc

    symbol :: Symbol
    symbol = intern ""
