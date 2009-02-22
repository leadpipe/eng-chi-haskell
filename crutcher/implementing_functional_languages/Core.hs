module Core where

import Data.List
import Data.Either
import Text.ParserCombinators.Parsec.Prim(parse)

import Expr
import Parser

import Heap

import Template

-- For main/debugging
import System.Environment
import Text.ParserCombinators.Parsec.Prim(parseTest)


main = do
  source <- getContents
  parseTest (only parseCoreProgram) source


corePrelude :: String
corePrelude = intercalate ";\n"
 ["I x = x",
  "K x y = x",
  "K1 x y = y",
  "S f g x = f x (g x)",
  "compose f g x = f (g x)",
  "twice f = compose f f"]

corePreludeProgram :: CoreProgram
corePreludeProgram
  = case (parse (only parseCoreProgram) "(prelude)" corePrelude) of 
      Left err -> error (show err)
      Right p -> p

simple = intercalate ";\n"
 ["main = I 1",
  "I x = x"]

simpleProgram :: CoreProgram
simpleProgram
  = case (parse (only parseCoreProgram) "(prelude)" simple) of 
      Left err -> error (show err)
      Right p -> p -- ++ corePreludeProgram


