{-# LANGUAGE QuasiQuotes #-}

module Objc (objcTests) where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit ((@=?))

import Language.C.Quote.ObjC

objcTests :: [Test]
objcTests = [objcProp, objcDict, objcParm, objcMethodDefinition, objcArgumentCls, objcArgument, objcArguments, objcVarArgument, objcLit] 

objcDict :: Test
objcDict = testCase "objc params" $
    [cexp| @{$dictelems:(elems [("a","b"),("c", "d")])} |] @=? [cexp| @{@"a" : @"b",@"c": @"d"} |]
    where elems = map (\(k,v) -> [ocdictelem|$exp:(toObjCLit k) : $exp:(toObjCLit v)|] ) 

objcProp :: Test
objcProp = testCase "objc property" $
    [cedecl|
     @interface Foo
     - (void) foo;
     $prop:propdec1
     $props:propdec2
     $prop:propdec3
     @end
     |] @=? [cedecl|
             @interface Foo
             - (void) foo;
             @property (nonatomic, retain) int i;
             @property (nonatomic, retain) float j;
             @property (nonatomic, retain) char k;
             @property (nonatomic) double l;
             @end
             |]
  where
    propdec n typ = [propdecl|@property ($propattrs:r) $ty:typ $id:n;|]
    propdec' n typ = [propdecl|@property ($propattr:p) $ty:typ $id:n;|]
    p = [ocpropattr|nonatomic|]
    q = [ocpropattr|retain|]
    r = [p,q]
    propdec1 = propdec "i" [cty|int|]
    propdec2 = map (\(n,t) -> propdec n t) [("j", [cty|float|]), ("k", [cty|char|])]
    propdec3 = propdec' "l" [cty|double|]

objcParm :: Test
objcParm = testCase "objc method parameters" $
    [cedecl|
    @interface Foo
    - (void) foo:(int)str fo:(int)str1; 
    + (int) test1:(int)str2; 
    @end
    |] @=? [cedecl|
            @interface Foo
            - (void) $methodparams:paramNew ;
            $methodproto:val ;
            @end
            |]

    where paramNew1 = [ocmethodparam|$id:("foo"):(int)str |]
          paramNew2 = [ocmethodparam|fo:(int)str1 |]
          paramNew3 = [ocmethodparam|test1:(int)str2 |]
          paramNew = [paramNew1, paramNew2]
          val = [ocmethodproto|+ (int) $methodparam:paramNew3|]

objcMethodDefinition :: Test
objcMethodDefinition = testCase "Objective C Method definition" $
     [cedecl|@implementation fooclass
             + (int) test1:(int)foo { }  
             - (char) test2:(char)bar { }  
             + (float) test3:(double)baz { }  
             @end
     |] @=? [cedecl|@implementation fooclass
                    $methoddefs:(val)
                    $methoddef:(val3)
                    @end
            |]
     where
        val3 = [ocmethoddef|+ (float) $methodparam:paramNew5 {} |]
        paramNew5 = [ocmethodparam|test3:(double)baz |]
        val2 = [ocmethoddef|+ (int) $methodparam:paramNew3 {} |]
        paramNew3 = [ocmethodparam|test1:(int)foo |]
        val1 = [ocmethoddef|- (char) $methodparam:paramNew4 {} |]
        paramNew4 = [ocmethodparam|test2:(char)bar |]
        val = [val2, val1]

objcArgumentCls :: Test
objcArgumentCls = testCase "Objc classmethod" $
    [citem|[somename test];|] @=? [citem|[$recv:(k) $id:("test")];|]
    where
        k = [ocmethodrecv|somename|]

objcArgument :: Test
objcArgument = testCase "Objc argument" $
    [citem|[somename doSome:@"string"];|] @=? [citem|[$recv:(k) $kwarg:(p)];|]
    where
        k = [ocmethodrecv|somename|]
        p = [ocarg|doSome:@"string"|]

objcArguments :: Test
objcArguments = testCase "Objc arguments" $
    [citem|[somename doSome:@"string" doSomeMore:@"moreStrings"];|] @=? [citem|[$recv:(k) $kwargs:(r)];|]
    where
        k = [ocmethodrecv|somename|]
        p = [ocarg|doSome:@"string"|]
        q = [ocarg|doSomeMore:@"moreStrings"|]
        r = [p,q]

objcVarArgument :: Test
objcVarArgument = testCase "Objc varargument" $
    [citem|[NSString stringWithFormat:@"A string: %@, a float: %1.2f", @"string", 31415.9265];
    |] @=? [citem|[$recv:(k) $kwarg:(r) $args:(p)];|]
    where
        k = [ocmethodrecv|NSString|]
        r = [ocarg|stringWithFormat:@"A string: %@, a float: %1.2f"|]
        p = [a, b]
        a = [cexp|@"string"|]
        b = [cexp|31415.9265|]

objcLit :: Test
objcLit = testCase "Objc To Literals" $
    [cexp|@[ @"foo"
           , @YES
           , @NO
           , @'a'
           , nil] |] @=? 
    [cexp|@[ $exp:(toObjCLit "foo")
           , $exp:(toObjCLit True)
           , $exp:(toObjCLit False)
           , $exp:(toObjCLit 'a')
           , nil] |]
