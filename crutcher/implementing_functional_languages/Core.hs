module Core where

import Data.List
import Data.Either
import Text.ParserCombinators.Parsec.Prim(parse)

import Expr
import Parser

-- For main/debugging
import System.Environment
import Text.ParserCombinators.Parsec.Prim(parseTest)


corePrelude :: String
corePrelude = intercalate ";\n"
   ["I x = x",
    "K x y = x",
    "K1 x y = y",
    "S f g x = f x (g x)",
    "compose f g x = f (g x)",
    "twice f = compose f f"]

corePreludeProgram :: CoreProgram
corePreludeProgram = case (parse parseCoreProgram "prelude" corePrelude) of 
  Left err -> error (show err)
  Right p -> p
