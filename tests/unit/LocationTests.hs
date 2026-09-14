{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

module LocationTests (locationTests) where

import qualified Data.ByteString.Char8     as B
import           Data.Loc
import qualified Language.C.Parser         as P
import           Language.C.Quote.C        (cexp)
import qualified Language.C.Syntax         as C
import qualified Language.Haskell.TH       as TH
import           Language.Haskell.TH.Quote (quoteExp)
import           Test.Tasty
import           Test.Tasty.HUnit

locationTests :: TestTree
locationTests = testGroup "Source locations"
    [ testCase "known offsets across tabs and newlines" $ do
        expr <- parseExp "\tfoo\n + bar" (Just (startPos "input.c"))
        checkSpan expr ("input.c", 1, 9, Just 1) ("input.c", 2, 7, Just 11)
    , testCase "absent starting position" $ do
        expr <- parseExp "x" Nothing
        locOf expr @?= NoLoc
    , testCase "line-only starting position" $ do
        expr <- parseExp " x" (Just (linePos "input.c" 10))
        checkSpan expr ("input.c", 10, 2, lineOffset 1)
                       ("input.c", 10, 3, lineOffset 2)
    , testCase "line directive" (lineDirective "#line")
    , testCase "preprocessor line marker" (lineDirective "#")
    , testCase "quotation coordinates and offsets" $ do
        let ((file, line, col), expr) = quotedExpression
        checkSpan expr (file, line, col, lineOffset 0)
                       (file, line, col + 1, lineOffset 1)
    , testCase "relocation replaces only the outer location" $ do
        expr <- parseExp "x" (Just (startPos "input.c"))
        let start = linePos "relocated.c" 20
            moved = reloc (Loc start (advancePos start 'x')) expr
        checkSpan moved ("relocated.c", 20, 1, lineOffset 0)
                        ("relocated.c", 20, 2, lineOffset 1)
        case moved of
          C.Var ident _ -> checkSpan ident ("input.c", 1, 1, Just 0)
                                          ("input.c", 1, 2, Just 1)
          _ -> assertFailure "Expected a variable expression"
    ]
  where
    lineDirective prefix = do
        expr <- parseExp (prefix ++ " 40 \"mapped.c\"\n\tx")
                         (Just (startPos "input.c"))
        checkSpan expr ("mapped.c", 40, 9, lineOffset 2)
                       ("mapped.c", 40, 10, lineOffset 3)

type Position = (FilePath, Int, Int, Maybe Int)

checkSpan :: Located a => a -> Position -> Position -> Assertion
checkSpan value start end =
    case locOf value of
      Loc p q -> (position p, position q) @?= (start, end)
      NoLoc   -> assertFailure "Expected a source span"
  where
    position p = (posFile p, posLine p, posCol p, offset p)

offset :: Pos -> Maybe Int
#if MIN_VERSION_srcloc(0,7,0)
offset = posCoff
#else
offset = Just . posCoff
#endif

-- Older srcloc versions cannot represent an unknown offset.
lineOffset :: Int -> Maybe Int
#if MIN_VERSION_srcloc(0,7,0)
lineOffset _ = Nothing
#else
lineOffset n = Just n
#endif

parseExp :: String -> Maybe Pos -> IO C.Exp
parseExp source start =
    either (fail . show) return $ P.parse [] [] P.parseExp (B.pack source) start

quotedExpression :: ((FilePath, Int, Int), C.Exp)
quotedExpression = $(do
    loc <- TH.location
    let file = TH.loc_filename loc
        (line, col) = TH.loc_start loc
    [| ((file, line, col), $(quoteExp cexp "x")) |])
