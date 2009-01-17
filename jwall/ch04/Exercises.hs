--file: ch04/Exercises.hs
import Data.Char (digitToInt)

data Item a = Item a
            | Undefined
            deriving (Show)

--safeHead

safeHead [] = Undefined
safeHead (x:xs) = Item x

--safeTail

safeTail [] = []
safeTail (x:xs) = xs

--safeLast

safeLast [] = Undefined
safeLast (x:[]) = Item x
safeLast (x:xs) = safeLast(xs)

--safeInit

safeInit [] = [Undefined]
safeInit (x1:x2:[]) = [Item x1]
safeInit (x:xs) = Item x:safeInit(xs)

--fold asInt

myAsInt :: [a] -> Int
myAsInt xs = foldr (accFun) 0 xs
    where 
          accFun '-' acc = 0 - acc
          accFun x acc = acc * 10 + digitToInt x

myConcat :: [[a]] -> [a]
myConcat ll = foldr (accFun) [] ll
    where
          accFun x acc = x ++ acc 
