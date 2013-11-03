{-# LANGUAGE QuasiQuotes #-}

module Objc (objcTests) where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit (Assertion, (@?=))

import Language.C.Quote.ObjC

objcTests :: Test
objcTests = testGroup "Objective-C"
    [ testCase "Objective-C params" objcProp
    , testCase "Objective-C property" objcDict
    , testCase "Objective-C method parameters" objcParam
    , testCase "Objective-C method definition" objcMethodDefinition
    , testCase "Objective-C classmethod" objcArgumentCls
    , testCase "Objective-C argument" objcArgument
    , testCase "Objective-C arguments" objcArguments
    , testCase "Objective-C varargument" objcVarArgument
    , testCase "Objective-C literals" objcLits
    ]
  where
    objcDict :: Assertion
    objcDict =
        [cexp| @{$dictelems:(elems [("a","b"),("c", "d")])} |] @?= [cexp| @{@"a" : @"b",@"c": @"d"}|]
      where
        elems = map (\(k,v) -> [objcdictelem|$exp:(objcLit k) : $exp:(objcLit v)|] )

    objcProp :: Assertion
    objcProp =
        [cedecl|
         @interface Foo
         - (void) foo;
         $prop:propdec1
         $props:propdec2
         $prop:propdec3
         @end
         |]

        @?=

        [cedecl|
        @interface Foo
        - (void) foo;
        @property (nonatomic, retain) int i;
        @property (nonatomic, retain) float j;
        @property (nonatomic, retain) char k;
        @property (nonatomic) double l;
        @end
        |]
      where
        propdec n typ = [objcprop|@property ($propattrs:r) $ty:typ $id:n;|]
        propdec' n typ = [objcprop|@property ($propattr:p) $ty:typ $id:n;|]
        p = [objcpropattr|nonatomic|]
        q = [objcpropattr|retain|]
        r = [p,q]
        propdec1 = propdec "i" [cty|int|]
        propdec2 = map (\(n,t) -> propdec n t) [("j", [cty|float|]), ("k", [cty|char|])]
        propdec3 = propdec' "l" [cty|double|]

    objcParam :: Assertion
    objcParam =
        [cedecl|
        @interface Foo
        - (void) $methparams:paramNew ;
        $methproto:val ;
        @end
        |]

        @?=

        [cedecl|
        @interface Foo
        - (void) foo:(int)str fo:(int)str1;
        + (int) test1:(int)str2;
        @end
        |]
      where
        paramNew1 = [objcmethparam|$id:("foo"):(int)str |]
        paramNew2 = [objcmethparam|fo:(int)str1 |]
        paramNew3 = [objcmethparam|test1:(int)str2 |]
        paramNew = [paramNew1, paramNew2]
        val = [objcmethproto|+ (int) $methparam:paramNew3|]

    objcMethodDefinition :: Assertion
    objcMethodDefinition =
        [cedecl|
        @implementation fooclass
        $methdefs:(val)
        $methdef:(val3)
        @end
        |]

        @?=

        [cedecl|
        @implementation fooclass
        + (int) test1:(int)foo { }
        - (char) test2:(char)bar { }
        + (float) test3:(double)baz { }
        @end
        |]
      where
        val3 = [objcmethdef|+ (float) $methparam:paramNew5 {} |]
        paramNew5 = [objcmethparam|test3:(double)baz |]
        val2 = [objcmethdef|+ (int) $methparam:paramNew3 {} |]
        paramNew3 = [objcmethparam|test1:(int)foo |]
        val1 = [objcmethdef|- (char) $methparam:paramNew4 {} |]
        paramNew4 = [objcmethparam|test2:(char)bar |]
        val = [val2, val1]

    objcArgumentCls :: Assertion
    objcArgumentCls =
        [citem|[somename test];|] @?= [citem|[$recv:(k) $id:("test")];|]
      where
        k = [objcmethrecv|somename|]

    objcArgument :: Assertion
    objcArgument =
        [citem|[$recv:(k) $kwarg:(p)];|] @?= [citem|[somename doSome:@"string"];|]
      where
        k = [objcmethrecv|somename|]
        p = [objcarg|doSome:@"string"|]

    objcArguments :: Assertion
    objcArguments =
        [citem|[$recv:(k) $kwargs:(r)];|]
        @?= [citem|[somename doSome:@"string" doSomeMore:@"moreStrings"];|]
      where
        k = [objcmethrecv|somename|]
        p = [objcarg|doSome:@"string"|]
        q = [objcarg|doSomeMore:@"moreStrings"|]
        r = [p,q]

    objcVarArgument :: Assertion
    objcVarArgument =
        [citem|[$recv:(k) $kwarg:(r) $args:(p)];|]
        @?= [citem|[NSString stringWithFormat:@"A string: %@, a float: %1.2f", @"string", 31415.9265];|]
      where
        k = [objcmethrecv|NSString|]
        r = [objcarg|stringWithFormat:@"A string: %@, a float: %1.2f"|]
        p = [a, b]
        a = [cexp|@"string"|]
        b = [cexp|31415.9265|]

    objcLits :: Assertion
    objcLits =
        [cexp|@[$(objcLit "foo"), $(objcLit True), $(objcLit False), $(objcLit 'a'), nil]|]
        @?= [cexp|@[@"foo", @YES, @NO, @'a', nil]|]
