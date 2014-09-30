import Control.Exception
import Control.Monad (when)
import qualified Data.ByteString.Char8 as B
import Data.Loc
import System.Environment (getArgs)
import Text.PrettyPrint.Mainland

import qualified Language.C.Parser as P
import qualified Language.C.Parser.Tokens as T
import qualified Language.C.Syntax as C
import Language.C.Properties

import Opts

extsMap :: [(Flag, C.Extensions)]
extsMap = [(C99,    C.C99)
          ,(C11,    C.C11)
          ,(Gcc,    C.Gcc)
          ,(Blocks, C.Blocks)
          ,(ObjC,   C.ObjC)
          ,(CUDA,   C.CUDA)
          ]

main :: IO ()
main = do
    args <- getArgs
    (flags, files) <- compilerOpts args
    let exts = [ext | (f, ext) <- extsMap, f `elem` flags]
    let doTokens = Tokens `elem` flags
    case length files of
      0 -> return ()
      _ -> do  when doTokens $ mapM_ (lexFile exts) files
               mapM_ (parseFile flags exts) files

lexFile :: [C.Extensions] -> String -> IO ()
lexFile exts filename = do
    buf <- B.readFile filename
    case tokens buf of
      Left err -> fail $ show err
      Right ts -> mapM_ print ts
  where
    tokens :: B.ByteString -> Either SomeException [L T.Token]
    tokens buf = P.evalP tokensP (P.emptyPState exts [] buf start)

    start :: Pos
    start = startPos filename

    tokensP :: P.P [L T.Token]
    tokensP = do
        t <- P.lexToken
        case t of
          L _ T.Teof  -> return []
          _           -> tokensP >>= \ts -> return (t : ts)

parseFile :: [Flag] -> [C.Extensions] -> String -> IO ()
parseFile flags exts filename = do
    s <- B.readFile filename
    case P.parse exts [] P.parseUnit s start of
      Left err   -> fail $ show err
      Right defs -> if doPrint
                    then if doPrama
                         then putStr $ prettyPragma 80 (ppr defs)
                         else putStr $ pretty 80 (ppr defs)
                    else return ()
    when (not (prop_ParsePrintUnitId exts s)) $
        putStrLn $ "Bad pretty-printing: " ++ filename
  where
    doPrint :: Bool
    doPrint = Print `elem` flags

    doPrama :: Bool
    doPrama = Pragma `elem` flags

    start :: Pos
    start = startPos filename
