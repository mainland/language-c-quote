{-# LANGUAGE QuasiQuotes #-}

module Objc (objcTests) where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit ((@=?))

import Language.C.Quote.ObjC

objcTests :: [Test]
objcTests = [objcProp]

objcProp = testCase "objc property" $
    [cedecl|
     @interface Foo
     - (void) foo;
     $prop:propdec1
     $props:propdec2
     @end
     |] @=? [cedecl|
             @interface Foo
             - (void) foo;
             @property int i;
             @property float j;
             @property char k;
             @end
             |]
  where
    propdec n typ = [propdecl|@property $ty:typ $id:n;|]
    propdec1 = propdec "i" [cty|int|]
    propdec2 = map (\(n,t) -> propdec n t) [("j", [cty|float|]), ("k", [cty|char|])]
