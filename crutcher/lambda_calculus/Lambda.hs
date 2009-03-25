module Main where

data LExpr
  = Const Int
  | Var String
  | App LExpr LExpr
  | Lam String LExpr
  deriving (Show)

syms :: [String]
syms = [ xs ++ [x] | xs <- [] : syms, x <- ['a'..'z']]

