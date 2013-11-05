import Control.Exception
import Control.Monad (mapM_,
                      when)
import qualified Data.ByteString.Char8 as B
import Data.Loc
import Data.Symbol
import System.IO
import System.Environment (getArgs)
import Text.PrettyPrint.Mainland

import qualified Language.C.Parser as P
import qualified Language.C.Parser.Tokens as T
import qualified Language.C.Syntax as C
import Language.C.Properties

import Opts

main :: IO ()
main = do
    args <- getArgs
    (flags, files) <- compilerOpts args
    let exts = (if C99 `elem` flags then [C.C99] else []) ++
               (if C11 `elem` flags then [C.C11] else []) ++
               (if Gcc `elem` flags then [C.Gcc] else []) ++
               (if CUDA `elem` flags then [C.CUDA] else [])
    let doPrint = Print `elem` flags
    let doTokens = Tokens `elem` flags
    case length files of
      0 -> return ()
      _ -> do  when doTokens $ mapM_ (lexFile exts) files
               mapM_ (parseFile doPrint exts) files

lexFile :: [C.Extensions] -> String -> IO ()
lexFile exts filename = do
    buf <- B.readFile filename
    case ts buf of
      Left err -> fail $ show err
      Right ts -> mapM_ print ts
  where
    ts :: B.ByteString -> Either SomeException [L T.Token]
    ts buf = P.evalP tokens (P.emptyPState exts [] buf start)

    start :: Pos
    start = startPos filename

    tokens :: P.P [L T.Token]
    tokens = do
        t <- P.lexToken
        case t of
          L _ T.Teof  -> return []
          _           -> tokens >>= \ts -> return (t : ts)

parseFile :: Bool -> [C.Extensions] -> String -> IO ()
parseFile doPrint exts filename = do
    s <- B.readFile filename
    case P.parse exts [] P.parseUnit s start of
      Left err   -> fail $ show err
      Right defs -> if doPrint
                    then putStr $ prettyPragma 80 (ppr defs)
                    else return ()
    when (not (prop_ParsePrintUnitId exts s)) $
        putStrLn $ "Bad pretty-printing: " ++ filename
  where
    start :: Pos
    start = startPos filename

    posToString :: Pos -> String
    posToString p = "#line " ++ show (posLine p) ++ " " ++ show (posFile p)
