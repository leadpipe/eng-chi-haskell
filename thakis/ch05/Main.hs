module Main () where

import SimpleJSON
import Prettify
import PrettyJSON

main = do
  obj <- JObject [("foo", JNumber 1), ("bar", JBool False)]
  print obj
  putStrLn $ pretty $ renderJValue obj

