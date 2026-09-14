module PrettyTests (prettyTests) where

import qualified Data.ByteString.Char8           as B
import           Data.Loc                        (startPos)
import qualified Language.C.Parser               as P
import qualified Language.C.Syntax               as C
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit                      ((@?=))
import           Text.PrettyPrint.Mainland
import           Text.PrettyPrint.Mainland.Class

prettyTests :: Test
prettyTests = testGroup "Pretty printing" $
    [ testCase ("round trip at width " ++ show width) (roundTrip (pretty width))
    | width <- [0, 12, 80]
    ] ++
    [ testCase "compact round trip" (roundTrip prettyCompact)
    , testCase "source pragma" $ do
        defs <- parseUnit "int x;"
        prettyPragma 80 (ppr defs) @?= "#line 1 \"pretty-test.c\"\nint x;\n"
    ]
  where
    roundTrip render = do
        expected <- parseUnit source
        actual <- parseUnit (render (ppr expected))
        actual @?= expected

    source = "int values[1] = {1 + 2 * 3};\n" ++
             "int f(int x) { /* keep */ return (x + 1) * 2; }"

parseUnit :: String -> IO [C.Definition]
parseUnit source =
    either (fail . show) return $
    P.parse [] [] P.parseUnit (B.pack source) (Just (startPos "pretty-test.c"))
