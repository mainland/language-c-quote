-- |
-- Module      :  Language.C.Quote
-- Copyright   :  (c) Harvard University 2006-2011
--                (c) Geoffrey Mainland 2011-2013
-- License     :  BSD-style
-- Maintainer  :  mainland@eecs.harvard.edu

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -w #-}

module Language.C.Quote.Base (
    ToExp(..),
    quasiquote
  ) where

import Control.Monad ((>=>))
import qualified Data.ByteString.Char8 as B
import Data.Data (Data(..))
import Data.Generics (extQ)
import Data.Loc
import Data.Typeable (Typeable(..))
import Language.Haskell.Meta (parseExp, parsePat)
import Language.Haskell.TH
#if MIN_VERSION_template_haskell(2,7,0)
import Language.Haskell.TH.Quote (QuasiQuoter(..),
                                  dataToQa,
                                  dataToExpQ,
                                  dataToPatQ)
#else /* !MIN_VERSION_template_haskell(2,7,0) */
import Language.Haskell.TH.Quote (QuasiQuoter(..))
#endif /* !MIN_VERSION_template_haskell(2,7,0) */
import Language.Haskell.TH.Syntax

import qualified Language.C.Parser as P
import qualified Language.C.Syntax as C

class ToIdent a where
    toIdent :: a -> SrcLoc -> C.Id

instance ToIdent C.Id where
    toIdent ident _ = ident

instance ToIdent (SrcLoc -> C.Id) where
    toIdent ident = ident

instance ToIdent String where
    toIdent s loc = C.Id s loc

class ToExp a where
    toExp :: a -> SrcLoc -> C.Exp

instance ToExp C.Exp where
    toExp e _ = e

instance ToExp Int where
    toExp n loc = C.Const (C.IntConst (show n) C.Signed (fromIntegral n) loc) loc

instance ToExp Integer where
    toExp n loc = C.Const (C.IntConst (show n) C.Signed n loc) loc

instance ToExp Rational where
    toExp n loc = C.Const (C.DoubleConst (show n) n loc) loc

instance ToExp Float where
    toExp n loc = C.Const (C.DoubleConst (show n) (toRational n) loc) loc

instance ToExp Double where
    toExp n loc = C.Const (C.DoubleConst (show n) (toRational n) loc) loc

instance ToExp Char where
    toExp c loc = C.Const (C.CharConst (show c) c loc) loc

instance ToExp String where
    toExp s loc = C.Const (C.StringConst [show s] s loc) loc

antiVarE :: String -> ExpQ
antiVarE = either fail return . parseExp

qqLocE :: SrcLoc -> ExpQ
qqLocE loc = dataToExpQ qqExp loc

qqStringE :: String -> Maybe (Q Exp)
qqStringE s = Just $ litE $ stringL s

qqIdE :: C.Id -> Maybe (Q Exp)
qqIdE (C.AntiId v loc)  = Just [|toIdent $(antiVarE v) $(qqLocE loc)|]
qqIdE _                 = Nothing

qqDeclSpecE :: C.DeclSpec -> Maybe (Q Exp)
qqDeclSpecE (C.AntiDeclSpec v _) = Just $ antiVarE v
qqDeclSpecE (C.AntiTypeDeclSpec extraStorage extraTypeQuals v _) =
    Just [|let C.Type (C.DeclSpec storage typeQuals typeSpec loc) _ _
                   = $(antiVarE v)
           in
             C.DeclSpec (storage ++ $(dataToExpQ qqExp extraStorage))
                        (typeQuals ++ $(dataToExpQ qqExp extraTypeQuals))
                        typeSpec
                        loc
         |]
qqDeclSpecE _ = Nothing

qqDeclE :: C.Decl -> Maybe (Q Exp)
qqDeclE (C.AntiTypeDecl v _) =
    Just [|let C.Type _ decl _ = $(antiVarE v) in decl|]
qqDeclE _ = Nothing

qqTypeE :: C.Type -> Maybe (Q Exp)
qqTypeE (C.AntiType v _)  = Just $ antiVarE v
qqTypeE _                 = Nothing

qqInitializerE :: C.Initializer -> Maybe (Q Exp)
qqInitializerE (C.AntiInit v _)  = Just $ antiVarE v
qqInitializerE _                 = Nothing

qqInitializerListE :: [(Maybe C.Designation, C.Initializer)] -> Maybe (Q Exp)
qqInitializerListE [] = Just [|[]|]
qqInitializerListE ((Nothing, C.AntiInits v _) : fields) =
    Just [|[(Nothing, init) | init <- $(antiVarE v)] ++ $(dataToExpQ qqExp fields)|]
qqInitializerListE (field : fields) =
    Just [|$(dataToExpQ qqExp field) : $(dataToExpQ qqExp fields)|]

qqInitGroupE :: C.InitGroup -> Maybe (Q Exp)
qqInitGroupE (C.AntiDecl v _)  = Just $ antiVarE v
qqInitGroupE _                 = Nothing

qqInitGroupListE :: [C.InitGroup] -> Maybe (Q Exp)
qqInitGroupListE [] = Just [|[]|]
qqInitGroupListE (C.AntiDecls v _ : inits) =
    Just [|$(antiVarE v) ++ $(dataToExpQ qqExp inits)|]
qqInitGroupListE (ini : inis) =
    Just [|$(dataToExpQ qqExp ini) : $(dataToExpQ qqExp inis)|]

qqFieldGroupE :: C.FieldGroup -> Maybe (Q Exp)
qqFieldGroupE (C.AntiSdecl v _)  = Just $ antiVarE v
qqFieldGroupE _                  = Nothing

qqFieldGroupListE :: [C.FieldGroup] -> Maybe (Q Exp)
qqFieldGroupListE [] = Just [|[]|]
qqFieldGroupListE (C.AntiSdecls v _ : fields) =
    Just [|$(antiVarE v) ++ $(dataToExpQ qqExp fields)|]
qqFieldGroupListE (field : fields) =
    Just [|$(dataToExpQ qqExp field) : $(dataToExpQ qqExp fields)|]

qqCEnumE :: C.CEnum -> Maybe (Q Exp)
qqCEnumE (C.AntiEnum v _)  = Just $ antiVarE v
qqCEnumE _                 = Nothing

qqCEnumListE :: [C.CEnum] -> Maybe (Q Exp)
qqCEnumListE [] = Just [|[]|]
qqCEnumListE (C.AntiEnums v _ : fields) =
    Just [|$(antiVarE v) ++ $(dataToExpQ qqExp fields)|]
qqCEnumListE (field : fields) =
    Just [|$(dataToExpQ qqExp field) : $(dataToExpQ qqExp fields)|]

qqParamE :: C.Param -> Maybe (Q Exp)
qqParamE (C.AntiParam v _)  = Just $ antiVarE v
qqParamE _                  = Nothing

qqParamListE :: [C.Param] -> Maybe (Q Exp)
qqParamListE [] = Just [|[]|]
qqParamListE (C.AntiParams v _ : args) =
    Just [|$(antiVarE v) ++ $(dataToExpQ qqExp args)|]
qqParamListE (arg : args) =
    Just [|$(dataToExpQ qqExp arg) : $(dataToExpQ qqExp args)|]

qqDefinitionE :: C.Definition -> Maybe (Q Exp)
qqDefinitionE (C.AntiFunc v loc) =
    Just [|C.FuncDef $(antiVarE v) $(qqLocE loc)|]
qqDefinitionE (C.AntiEsc v loc) =
    Just [|C.EscDef $(antiVarE v) $(qqLocE loc)|]
qqDefinitionE (C.AntiEdecl v _) =
    Just $ antiVarE v
qqDefinitionE (C.AntiObjCMeth m _) =
    Just $ antiVarE m
qqDefinitionE _ = Nothing

qqDefinitionListE :: [C.Definition] -> Maybe (Q Exp)
qqDefinitionListE [] = Just [|[]|]
qqDefinitionListE (C.AntiEdecls v _ : defs) =
    Just [|$(antiVarE v) ++ $(dataToExpQ qqExp defs)|]
qqDefinitionListE (C.AntiObjCMeths m _ : meths) =
    Just [|$(antiVarE m) ++ $(dataToExpQ qqExp meths)|]
qqDefinitionListE (def : defs) =
    Just [|$(dataToExpQ qqExp def) : $(dataToExpQ qqExp defs)|]

qqConstE :: C.Const -> Maybe (Q Exp)
qqConstE = go
  where
    go (C.AntiInt v loc) =
        Just [|C.IntConst  $(intConst (antiVarE v)) C.Signed
                           (fromIntegral $(antiVarE v))
                           $(qqLocE loc)|]

    go (C.AntiUInt v loc) =
        Just [|C.IntConst  ($(intConst (antiVarE v)) ++ "U") C.Unsigned
                           (fromIntegral $(antiVarE v))
                           $(qqLocE loc)|]

    go (C.AntiLInt v loc) =
        Just [|C.LongIntConst  ($(intConst (antiVarE v)) ++ "L") C.Signed
                               (fromIntegral $(antiVarE v))
                               $(qqLocE loc)|]

    go (C.AntiULInt v loc) =
        Just [|C.LongIntConst  ($(intConst (antiVarE v)) ++ "UL") C.Unsigned
                               (fromIntegral $(antiVarE v))
                               $(qqLocE loc)|]

    go (C.AntiLLInt v loc) =
        Just [|C.LongIntConst  ($(intConst (antiVarE v)) ++ "LL") C.Signed
                               (fromIntegral $(antiVarE v))
                               $(qqLocE loc)|]

    go (C.AntiULLInt v loc) =
        Just [|C.LongIntConst  ($(intConst (antiVarE v)) ++ "ULL") C.Unsigned
                               (fromIntegral $(antiVarE v))
                               $(qqLocE loc)|]



    go (C.AntiFloat v loc) =
        Just [|C.FloatConst  ($(floatConst (antiVarE v)) ++ "F")
                             (fromRational $(antiVarE v))
                             $(qqLocE loc)|]

    go (C.AntiDouble v loc) =
        Just [|C.DoubleConst  ($(floatConst (antiVarE v)))
                              (fromRational $(antiVarE v))
                              $(qqLocE loc)|]

    go (C.AntiLongDouble v loc) =
        Just [|C.LongDoubleConst  ($(floatConst (antiVarE v)) ++ "L")
                                  (fromRational $(antiVarE v))
                                  $(qqLocE loc)|]

    go (C.AntiChar v loc) =
        Just [|C.CharConst (show $(antiVarE v)) $(antiVarE v) $(qqLocE loc)|]

    go (C.AntiString v loc) =
        Just [|C.StringConst [show $(antiVarE v)] $(antiVarE v) $(qqLocE loc)|]

    go _ = Nothing

    intConst :: ExpQ -> ExpQ
    intConst e = [|show $(e)|]

    floatConst :: ExpQ -> ExpQ
    floatConst e = [|show (fromRational $(e) :: Double)|]

qqExpE :: C.Exp -> Maybe (Q Exp)
qqExpE (C.AntiExp v loc) = Just [|toExp $(antiVarE v) $(qqLocE loc) :: C.Exp|]
qqExpE _                 = Nothing

qqExpListE :: [C.Exp] -> Maybe (Q Exp)
qqExpListE [] = Just [|[]|]
qqExpListE (C.AntiArgs v loc : exps) =
    Just [|map (uncurry toExp) ($(antiVarE v) `zip` repeat $(qqLocE loc)) ++
           $(dataToExpQ qqExp exps)|]
qqExpListE (exp : exps) =
    Just [|$(dataToExpQ qqExp exp) : $(dataToExpQ qqExp exps)|]

qqStmE :: C.Stm -> Maybe (Q Exp)
qqStmE (C.AntiPragma v loc) = Just [|C.Pragma $(antiVarE v) $(qqLocE loc)|]
qqStmE (C.AntiStm v _)      = Just $ antiVarE v
qqStmE _                    = Nothing

qqStmListE :: [C.Stm] -> Maybe (Q Exp)
qqStmListE [] = Just [|[]|]
qqStmListE (C.AntiStms v _ : stms) =
    Just [|$(antiVarE v) ++ $(dataToExpQ qqExp stms)|]
qqStmListE (stm : stms) =
    Just [|$(dataToExpQ qqExp stm) : $(dataToExpQ qqExp stms)|]

qqBlockItemE :: C.BlockItem -> Maybe (Q Exp)
qqBlockItemE (C.AntiBlockItem v _) = Just $ antiVarE v
qqBlockItemE _                     = Nothing

qqBlockItemListE :: [C.BlockItem] -> Maybe (Q Exp)
qqBlockItemListE [] = Just [|[]|]
qqBlockItemListE (C.BlockDecl (C.AntiDecls v _) : items) =
    Just [|map C.BlockDecl $(antiVarE v) ++ $(dataToExpQ qqExp items)|]
qqBlockItemListE (C.BlockStm (C.AntiStms v _) : items) =
    Just [|map C.BlockStm $(antiVarE v) ++ $(dataToExpQ qqExp items)|]
qqBlockItemListE (C.AntiBlockItems v _ : items) =
    Just [|$(antiVarE v) ++ $(dataToExpQ qqExp items)|]
qqBlockItemListE (stm : stms) =
    Just [|$(dataToExpQ qqExp stm) : $(dataToExpQ qqExp stms)|]


qqPropAttrE :: C.ObjCPropAttr -> Maybe (Q Exp)
qqPropAttrE (C.AntiAttr pa _) = Just $ antiVarE pa
qqPropAttrE _                  = Nothing

qqPropAttrListE :: [C.ObjCPropAttr] -> Maybe (Q Exp)
qqPropAttrListE (C.AntiAttrs pa _:attrelems) = Just $ [|$(antiVarE pa) ++ $(dataToExpQ qqExp attrelems)|]
qqPropAttrListE _                  = Nothing

qqDictsE :: [C.ObjcDictElem] -> Maybe (Q Exp)
qqDictsE (C.AntiDictElems e _:elems) = Just $ [|$(antiVarE e) ++ $(dataToExpQ qqExp elems)|]
qqDictsE _                  = Nothing

qqPropE :: C.ObjCIfaceDecl -> Maybe (Q Exp)
qqPropE (C.AntiProp p _) = Just $ antiVarE p
qqPropE _                  = Nothing

qqPropListE :: [C.ObjCIfaceDecl] -> Maybe (Q Exp)
qqPropListE [] = Just [|[]|]
qqPropListE (C.AntiProps p _: props) = Just $ [|$(antiVarE p) ++ $(dataToExpQ qqExp props)|]
qqPropListE _                 = Nothing

qqObjCParamE :: C.ObjCParm -> Maybe (Q Exp)
qqObjCParamE (C.AntiObjCParm p _) = Just $ antiVarE p
qqObjCParamE _                  = Nothing

qqObjCParamsE :: [C.ObjCParm] -> Maybe (Q Exp)
qqObjCParamsE [] = Just [|[]|]
qqObjCParamsE (C.AntiObjCParms p _: props) = Just $ [|$(antiVarE p) ++ $(dataToExpQ qqExp props)|]
qqObjCParamsE _                 = Nothing

qqObjCMethodProtoE :: C.ObjCMethodProto -> Maybe (Q Exp)
qqObjCMethodProtoE (C.AntiObjCMethodProto p _) = Just $ antiVarE p
qqObjCMethodProtoE _                  = Nothing

{-
qqObjCMethodDefnE :: C.Definition -> Maybe (Q Exp)
qqObjCMethodDefnE (C.AntiObjCMeth m _) = Just $ antiVarE m
qqObjCMethodDefnE _                  = Nothing

qqObjCMethodDefnsE :: [C.Definition] -> Maybe (Q Exp)
qqObjCMethodDefnsE [] = Just [|[]|]
qqObjCMethodDefnsE (C.AntiObjCMeths m _: defs) = Just $ [|$(antiVarE m) ++ $(dataToExpQ qqExp defs)|]
qqObjCMethodDefnsE _                 = Nothing
-}
qqExp :: Typeable a => a -> Maybe (Q Exp)
qqExp = const Nothing  `extQ` qqStringE
                       `extQ` qqIdE
                       --`extQ` qqObjCMethodDefnE
                       `extQ` qqDeclSpecE
                       `extQ` qqDeclE
                       `extQ` qqTypeE
                       `extQ` qqInitializerE
                       `extQ` qqInitializerListE
                       `extQ` qqInitGroupE
                       `extQ` qqInitGroupListE
                       `extQ` qqFieldGroupE
                       `extQ` qqFieldGroupListE
                       `extQ` qqCEnumE
                       `extQ` qqCEnumListE
                       `extQ` qqParamE
                       `extQ` qqParamListE
                       `extQ` qqDefinitionE
                       `extQ` qqDefinitionListE
                       `extQ` qqConstE
                       `extQ` qqExpE
                       `extQ` qqExpListE
                       `extQ` qqStmE
                       `extQ` qqStmListE
                       `extQ` qqBlockItemE
                       `extQ` qqBlockItemListE
                       `extQ` qqPropE
                       `extQ` qqPropListE
                       `extQ` qqDictsE
                       `extQ` qqPropAttrE
                       `extQ` qqPropAttrListE
                       `extQ` qqObjCParamE
                       `extQ` qqObjCParamsE
                       `extQ` qqObjCMethodProtoE
                       --`extQ` qqObjCMethodDefnsE

antiVarP :: String -> PatQ
antiVarP = either fail return . parsePat

qqStringP :: String -> Maybe (Q Pat)
qqStringP s = Just $ litP $ stringL s

qqLocP :: Data.Loc.Loc -> Maybe (Q Pat)
qqLocP _ = Just $ wildP

qqIdP :: C.Id -> Maybe (Q Pat)
qqIdP (C.AntiId v _) = Just $ conP (mkName "C.Id") [antiVarP v, wildP]
qqIdP _              = Nothing

qqDeclSpecP :: C.DeclSpec -> Maybe (Q Pat)
qqDeclSpecP (C.AntiDeclSpec v _) = Just $ antiVarP v
qqDeclSpecP (C.AntiTypeDeclSpec {}) =
    error "Illegal antiquoted type in pattern"
qqDeclSpecP _ = Nothing

qqDeclP :: C.Decl -> Maybe (Q Pat)
qqDeclP (C.AntiTypeDecl {}) =
    error "Illegal antiquoted type in pattern"
qqDeclP _ = Nothing

qqTypeP :: C.Type -> Maybe (Q Pat)
qqTypeP (C.AntiType v _)  = Just $ antiVarP v
qqTypeP _                 = Nothing

qqInitializerP :: C.Initializer -> Maybe (Q Pat)
qqInitializerP (C.AntiInit v _)  = Just $ antiVarP v
qqInitializerP _                 = Nothing

qqInitializerListP :: [C.Initializer] -> Maybe (Q Pat)
qqInitializerListP [] = Just $ listP []
qqInitializerListP [C.AntiInits v _] = Just $ antiVarP v
qqInitializerListP (C.AntiInits {} : _ : _) =
    error "Antiquoted list of initializers must be last item in quoted list"
qqInitializerListP (ini : inis) =
    Just $ conP (mkName ":") [dataToPatQ qqPat ini,  dataToPatQ qqPat inis]

qqInitGroupP :: C.InitGroup -> Maybe (Q Pat)
qqInitGroupP (C.AntiDecl v _) = Just $ antiVarP v
qqInitGroupP _                = Nothing

qqInitGroupListP :: [C.InitGroup] -> Maybe (Q Pat)
qqInitGroupListP [] = Just $ listP []
qqInitGroupListP [C.AntiDecls v _] = Just $ antiVarP v
qqInitGroupListP (C.AntiDecls {} : _ : _) =
    error "Antiquoted list of initialization groups must be last item in quoted list"
qqInitGroupListP (ini : inis) =
    Just $ conP (mkName ":") [dataToPatQ qqPat ini,  dataToPatQ qqPat inis]

qqFieldGroupP :: C.FieldGroup -> Maybe (Q Pat)
qqFieldGroupP (C.AntiSdecl v _) = Just $ antiVarP v
qqFieldGroupP _                 = Nothing

qqFieldGroupListP :: [C.FieldGroup] -> Maybe (Q Pat)
qqFieldGroupListP [] = Just $ listP []
qqFieldGroupListP [C.AntiSdecls v _] = Just $ antiVarP v
qqFieldGroupListP (C.AntiSdecls {} : _ : _) =
    error "Antiquoted list of struct/union fields must be last item in quoted list"
qqFieldGroupListP (ini : inis) =
    Just $ conP (mkName ":") [dataToPatQ qqPat ini,  dataToPatQ qqPat inis]

qqCEnumP :: C.CEnum -> Maybe (Q Pat)
qqCEnumP (C.AntiEnum v _) = Just $ antiVarP v
qqCEnumP _                = Nothing

qqCEnumListP :: [C.CEnum] -> Maybe (Q Pat)
qqCEnumListP [] = Just $ listP []
qqCEnumListP [C.AntiEnums v _] = Just $ antiVarP v
qqCEnumListP (C.AntiEnums {} : _ : _) =
    error "Antiquoted list of enumerations must be last item in quoted list"
qqCEnumListP (ini : inis) =
    Just $ conP (mkName ":") [dataToPatQ qqPat ini,  dataToPatQ qqPat inis]

qqParamP :: C.Param -> Maybe (Q Pat)
qqParamP (C.AntiParam v _) = Just $ antiVarP v
qqParamP _                 = Nothing

qqParamListP :: [C.Param] -> Maybe (Q Pat)
qqParamListP [] = Just $ listP []
qqParamListP [C.AntiParams v _] = Just $ antiVarP v
qqParamListP (C.AntiParams {} : _ : _) =
    error "Antiquoted list of parameters must be last item in quoted list"
qqParamListP (arg : args) =
    Just $ conP (mkName ":") [dataToPatQ qqPat arg,  dataToPatQ qqPat args]

qqDefinitionP :: C.Definition -> Maybe (Q Pat)
qqDefinitionP (C.AntiFunc v _)  = Just $ conP (mkName "C.FuncDef") [antiVarP v, wildP]
qqDefinitionP (C.AntiEsc v _)   = Just $ conP (mkName "C.EscDef") [antiVarP v, wildP]
qqDefinitionP (C.AntiEdecl v _) = Just $ antiVarP v
qqDefinitionP _                 = Nothing

qqDefinitionListP :: [C.Definition] -> Maybe (Q Pat)
qqDefinitionListP [] = Just $ listP []
qqDefinitionListP [C.AntiEdecls v _] = Just $ antiVarP v
qqDefinitionListP (C.AntiEdecls {} : _ : _) =
    error "Antiquoted list of definitions must be last item in quoted list"
qqDefinitionListP (arg : args) =
    Just $ conP (mkName ":") [dataToPatQ qqPat arg,  dataToPatQ qqPat args]

qqConstP :: C.Const -> Maybe (Q Pat)
qqConstP = go
  where
    go (C.AntiInt v _) =
        Just $ (con "C.IntConst") [wildP, signed, antiVarP v, wildP]
    go (C.AntiUInt v _) =
        Just $ (con "C.IntConst") [wildP, unsigned, antiVarP v, wildP]
    go (C.AntiLInt v _) =
        Just $ (con "C.LongIntConst") [wildP, signed, antiVarP v, wildP]
    go (C.AntiULInt v _) =
        Just $ (con "C.LongIntConst") [wildP, unsigned, antiVarP v, wildP]
    go (C.AntiFloat v _) =
        Just $ (con "C.FloatConst") [wildP, antiVarP v, wildP]
    go (C.AntiDouble v _) =
        Just $ (con "C.DoubleConst") [wildP, antiVarP v, wildP]
    go (C.AntiLongDouble v _) =
        Just $ (con "C.LongDoubleConst") [wildP, antiVarP v, wildP]
    go (C.AntiChar v _) =
        Just $ (con "C.CharConst") [wildP, antiVarP v, wildP]
    go (C.AntiString v _) =
        Just $ (con "C.StringConst") [wildP, antiVarP v, wildP]
    go _ =
        Nothing

    con n = conP (mkName n)

    signed   = conP (mkName "C.Signed") []
    unsigned = conP (mkName "C.Unsigned") []

qqExpP :: C.Exp -> Maybe (Q Pat)
qqExpP (C.AntiExp v _) = Just $ antiVarP v
qqExpP _               = Nothing

qqExpListP :: [C.Exp] -> Maybe (Q Pat)
qqExpListP [] = Just $ listP []
qqExpListP [C.AntiArgs v _] = Just $ antiVarP v
qqExpListP (C.AntiArgs {} : _ : _) =
    error "Antiquoted list of arguments must be last item in quoted list"
qqExpListP (arg : args) =
    Just $ conP (mkName ":") [dataToPatQ qqPat arg,  dataToPatQ qqPat args]

qqStmP :: C.Stm -> Maybe (Q Pat)
qqStmP (C.AntiStm v _) = Just $ antiVarP v
qqStmP _               = Nothing

qqStmListP :: [C.Stm] -> Maybe (Q Pat)
qqStmListP [] = Just $ listP []
qqStmListP [C.AntiStms v _] = Just $ antiVarP v
qqStmListP (C.AntiStms {} : _ : _) =
    error "Antiquoted list of statements must be last item in quoted list"
qqStmListP (arg : args) =
    Just $ conP (mkName ":") [dataToPatQ qqPat arg,  dataToPatQ qqPat args]

qqBlockItemP :: C.BlockItem -> Maybe (Q Pat)
qqBlockItemP (C.AntiBlockItem v _) = Just $ antiVarP v
qqBlockItemP _                     = Nothing

qqBlockItemListP :: [C.BlockItem] -> Maybe (Q Pat)
qqBlockItemListP [] = Just $ listP []
qqBlockItemListP (C.BlockDecl (C.AntiDecls {}) : _) =
    error "Antiquoted list of declarations cannot appear in block"
qqBlockItemListP (C.BlockStm (C.AntiStms {}) : _) =
    error "Antiquoted list of statements cannot appear in block"
qqBlockItemListP [C.AntiBlockItems v _] = Just $ antiVarP v
qqBlockItemListP (C.AntiBlockItems {} : _ : _) =
    error "Antiquoted list of block items must be last item in quoted list"
qqBlockItemListP (arg : args) =
    Just $ conP (mkName ":") [dataToPatQ qqPat arg,  dataToPatQ qqPat args]

qqPat :: Typeable a => a -> Maybe (Q Pat)
qqPat = const Nothing `extQ` qqStringP
                      `extQ` qqLocP
                      `extQ` qqIdP
                      `extQ` qqDeclSpecP
                      `extQ` qqDeclP
                      `extQ` qqTypeP
                      `extQ` qqInitializerP
                      `extQ` qqInitializerListP
                      `extQ` qqInitGroupP
                      `extQ` qqInitGroupListP
                      `extQ` qqFieldGroupP
                      `extQ` qqCEnumP
                      `extQ` qqCEnumListP
                      `extQ` qqParamP
                      `extQ` qqParamListP
                      `extQ` qqDefinitionP
                      `extQ` qqDefinitionListP
                      `extQ` qqConstP
                      `extQ` qqExpP
                      `extQ` qqExpListP
                      `extQ` qqStmP
                      `extQ` qqStmListP
                      `extQ` qqBlockItemP
                      `extQ` qqBlockItemListP

parse :: [C.Extensions]
      -> [String]
      -> P.P a
      -> String
      -> Q a
parse exts typenames p s = do
    loc <- location
    case P.parse (C.Antiquotation : exts) typenames p (B.pack s) (locToPos loc) of
      Left err -> fail (show err)
      Right x  -> return x
  where
    locToPos :: Language.Haskell.TH.Loc -> Pos
    locToPos loc = Pos (loc_filename loc)
                       ((fst . loc_start) loc)
                       ((snd . loc_start) loc)
                       0

quasiquote :: Data a
           => [C.Extensions]
           -> [String]
           -> P.P a
           -> QuasiQuoter
quasiquote exts typenames p =
    QuasiQuoter { quoteExp  = parse exts typenames p >=> dataToExpQ qqExp
                , quotePat  = parse exts typenames p >=> dataToPatQ qqPat
                , quoteType = fail "C type quasiquoter undefined"
                , quoteDec  = fail "C declaration quasiquoter undefined"
                }

#if !MIN_VERSION_template_haskell(2,7,0)
dataToQa  ::  forall a k q. Data a
          =>  (Name -> k)
          ->  (Lit -> Q q)
          ->  (k -> [Q q] -> Q q)
          ->  (forall b . Data b => b -> Maybe (Q q))
          ->  a
          ->  Q q
dataToQa mkCon mkLit appCon antiQ t =
    case antiQ t of
      Nothing ->
          case constrRep constr of
            AlgConstr _  ->
                appCon con conArgs
            IntConstr n ->
                mkLit $ integerL n
            FloatConstr n ->
                mkLit $ rationalL (toRational n)
            CharConstr c ->
                mkLit $ charL c
        where
          constr :: Constr
          constr = toConstr t

          con :: k
          con = mkCon (mkConName mod occ)
            where
              mod :: String
              mod = (tyconModule . dataTypeName . dataTypeOf) t

              occ :: String
              occ = showConstr constr

              mkConName :: String -> String -> Name
              mkConName "Prelude" "(:)" = Name (mkOccName ":") NameS
              mkConName "Prelude" "[]"  = Name (mkOccName "[]") NameS
              mkConName "Prelude" "()"  = Name (mkOccName "()") NameS

              mkConName "Prelude" s@('(' : ',' : rest) = go rest
                where
                  go :: String -> Name
                  go (',' : rest) = go rest
                  go ")"          = Name (mkOccName s) NameS
                  go _            = Name (mkOccName occ) (NameQ (mkModName mod))

              mkConName "GHC.Real" ":%" = mkNameG_d "base" "GHC.Real" ":%"

              mkConName mod occ = Name (mkOccName occ) (NameQ (mkModName mod))

          conArgs :: [Q q]
          conArgs = gmapQ (dataToQa mkCon mkLit appCon antiQ) t

      Just y -> y

-- | 'dataToExpQ' converts a value to a 'Q Exp' representation of the same
-- value. It takes a function to handle type-specific cases.
dataToExpQ  ::  Data a
            =>  (forall b . Data b => b -> Maybe (Q Exp))
            ->  a
            ->  Q Exp
dataToExpQ = dataToQa conE litE (foldl appE)

-- | 'dataToPatQ' converts a value to a 'Q Pat' representation of the same
-- value. It takes a function to handle type-specific cases.
dataToPatQ  ::  Data a
            =>  (forall b . Data b => b -> Maybe (Q Pat))
            ->  a
            ->  Q Pat
dataToPatQ = dataToQa id litP conP
#endif /* !MIN_VERSION_template_haskell(2,7,0) */
