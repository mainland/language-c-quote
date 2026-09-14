module ParserStateTests (parserStateTests) where

import           Control.Exception         (fromException)
import qualified Data.ByteString.Char8     as B
import           Language.C.Parser.Monad
import           Test.Tasty
import           Test.Tasty.HUnit
import           Text.PrettyPrint.Mainland (pretty)

parserStateTests :: TestTree
parserStateTests = testGroup "Parser state"
    [ testCase "lexer states are restored in stack order" $
        assertResult [0, 1, 2, 2, 1, 1, 0, 0, 3] $ sequence
            [ getLexState
            , pushLexState 1 >> getLexState
            , pushLexState 2 >> getLexState
            , popLexState
            , getLexState
            , popLexState
            , getLexState
            , popLexState
            , pushLexState 3 >> getLexState
            ]
    , testCase "nested scopes restore typedef and class names" $
        assertResult [outer, inner, neither, inner, outer] $ do
            addTypedef "Outer"
            addClassdef "OuterClass"
            a <- bindings
            pushScope
            addVariable "Outer"
            addVariable "OuterClass"
            addTypedef "Inner"
            addClassdef "InnerClass"
            b <- bindings
            pushScope
            addVariable "Inner"
            addVariable "InnerClass"
            c <- bindings
            popScope
            d <- bindings
            popScope
            e <- bindings
            return [a, b, c, d, e]
    , testCase "reading an empty lexer stack returns a parser error" $
        assertParserError "Cannot read an empty lexer state stack."
            (popLexState >> getLexState)
    , testCase "popping an empty lexer stack returns a parser error" $
        assertParserError "Cannot pop an empty lexer state stack."
            (popLexState >> popLexState)
    , testCase "popping an empty scope stack returns a parser error" $
        assertParserError "Cannot pop an empty scope stack." popScope
    ]
  where
    outer = ([True, False], [True, False])
    inner = ([False, True], [False, True])
    neither = ([False, False], [False, False])

    bindings = do
        types <- mapM isTypedef ["Outer", "Inner"]
        classes <- mapM isClassdef ["OuterClass", "InnerClass"]
        return (types, classes)

initialState :: PState
initialState = emptyPState [] [] B.empty Nothing

assertResult :: (Eq a, Show a) => a -> P a -> Assertion
assertResult expectedResult action =
    case evalP action initialState of
      Left err     -> assertFailure (show err)
      Right actual -> actual @?= expectedResult

assertParserError :: String -> P a -> Assertion
assertParserError message action =
    case evalP action initialState of
      Left err ->
          case fromException err of
            Just (ParserException _ doc) -> pretty 80 doc @?= message
            Nothing -> assertFailure $ "Expected ParserException, got " ++ show err
      Right _ -> assertFailure "Expected a parser error"
