{-# LANGUAGE QuasiQuotes #-}

module CUDA (cudaTests) where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit (Assertion, (@?=))

import Language.C.Quote.CUDA
import Language.C.Syntax
import Data.Loc (noLoc)

mkDeclarator params mutability = LambdaDeclarator (Params params False noLoc) mutability Nothing noLoc

mkIntroducer :: [CaptureListEntry] -> LambdaIntroducer
mkIntroducer mode = (LambdaIntroducer mode noLoc)

emptyLambda = lambdaByCapture []
lambdaByCapture captureMode = Lambda (mkIntroducer captureMode) Nothing [] noLoc
lambdaByCaptureBody captureMode statements = Lambda (mkIntroducer captureMode) Nothing statements noLoc
lambdaByCaptureParams captureMode params = Lambda (mkIntroducer captureMode) (Just $ mkDeclarator params False) [] noLoc

lambdaByParams params = Lambda (mkIntroducer []) (Just $ mkDeclarator params False) [] noLoc
mutableLambdaByParams params = Lambda (mkIntroducer []) (Just $ mkDeclarator params True) [] noLoc

cudaTests :: Test
cudaTests = testGroup "CUDA"
    $ map (testCase "lambda-expressions parsing") lambdas
  where
    lambdas :: [Assertion]
    lambdas = [ [cexp|[=] {}|] @?= lambdaByCapture [DefaultByValue]
              , [cexp|[&] {}|] @?= lambdaByCapture[DefaultByReference]
              , [cexp|[] {}|] @?= lambdaByCapture []
              , [cexp|[] {}|] @?= emptyLambda
              , [cexp|[] () {}|] @?= lambdaByParams []
              , [cexp|[] (int i) {}|] @?= lambdaByParams [param_int_i] 
              , [cexp|[] (int i, double j) {}|] @?= lambdaByParams [param_int_i, param_double_h] 
              , [cexp|[] ($param:param_int_i) {}|] @?= lambdaByParams [param_int_i] 
              , [cexp|[] (int i) mutable {}|] @?= mutableLambdaByParams [param_int_i]  
              , [cexp|[&] (int i) {}|] @?= lambdaByCaptureParams [DefaultByReference] [param_int_i] 
              , [cexp|[&] { $item:item_return_7 } |] @?= lambdaCapturingByRefAndReturning7
              , [cexp|[&] { return $exp:exp_7; } |] @?= lambdaCapturingByRefAndReturning7
              , [cexp|[]{}()|] @?= FnCall emptyLambda [] noLoc
              , [cexp|[](){}()|] @?= FnCall (lambdaByParams []) [] noLoc
              ]

    lambdaCapturingByRefAndReturning7 = lambdaByCaptureBody [DefaultByReference] [item_return_7]
    exp_7 = [cexp|7|]
    item_return_7 = [citem|return 7;|]
    param_int_i = [cparam|int i|]
    param_double_h = [cparam|double j|]