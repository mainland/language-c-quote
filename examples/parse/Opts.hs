module Opts where

import System.Console.GetOpt
import Data.Maybe (fromMaybe)

data Flag = C99
          | C11
          | Gcc
          | CUDA
          | Tokens
          | Print
          | Input String
          | Output String
  deriving (Eq, Show)

options :: [OptDescr Flag]
options =
    [ Option []    ["c99"]    (NoArg C99)          "support C99"
    , Option []    ["c11"]    (NoArg C11)          "support C11"
    , Option []    ["gcc"]    (NoArg Gcc)          "allow GCC extensions"
    , Option []    ["cuda"]   (NoArg CUDA)         "allow CUDA extensions"
    , Option []    ["tokens"] (NoArg Tokens)       "show tokens"
    , Option ['p'] ["print"]  (NoArg Print)        "pretty-print file"
    , Option ['o'] ["output"] (OptArg outp "FILE") "output FILE"
    ]

inp,outp :: Maybe String -> Flag
outp = Output . fromMaybe "stdout"
inp  = Input  . fromMaybe "stdin"

compilerOpts :: [String] -> IO ([Flag], [String])
compilerOpts argv =
    case getOpt Permute options argv of
      { (o,n,[]  ) -> return (o,n)
      ; (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
      }
  where
    header = "Usage: parse [OPTION...] files..."
