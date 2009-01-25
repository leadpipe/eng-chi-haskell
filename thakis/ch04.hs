import System.Environment (getArgs)

import Data.Char (digitToInt, isDigit)
import Data.List (transpose)

-- 1.1
safeHead :: [a] -> Maybe a
safeHead (x:xs) = Just x
safeHead _ = Nothing

safeTail :: [a] -> Maybe [a]
safeTail (x:xs) = Just xs
safeTail _ = Nothing

safeLast :: [a] -> Maybe a
safeLast (x:xs) = if null xs then Just x else safeLast xs
safeLast _ = Nothing

safeInit :: [a] -> Maybe [a]
safeInit a@(x:xs) = Just $ init a  -- :-P
safeInit _ = Nothing


-- 1.2

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith f (x:xs) =
  if f x then
    []:splitWith f xs
  else
    case splitWith f xs of
      (xs:xss) -> (x:xs):xss
      [] -> [[x]]
splitWith f [] = []


-- 1.3

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
  where
    mainWith function = do
      args <- getArgs
      case args of
        [input, output] -> interactWith function input output
        _ -> putStrLn "error: exactly two arguments needes"
    --myFunction = firstWords
    myFunction = transposeWords

firstWords text = unlines $ map firstWord $ lines text
  where firstWord line = case words line of
                           (x:xs) -> x
                           [] -> ""


-- 1.4

-- In Python, where zip can take any number of parameters, this could be
-- '\n'.join(zip(*lines.split(\n))). But this seems not to be possible.

transposeWords text = unlines $ transpose $ lines text  -- :-P


-- 2.1, 2.2 (, 2.3, 2.4)

-- Works like atoi(), i.e. parses as much integer as there is :-)
rawAsInt s = foldl (\a b -> 10*a + (digitToInt b)) 0 (takeWhile isDigit s)

asInt ('+':s) = rawAsInt s
asInt ('-':s) = -(rawAsInt s)
asInt s = rawAsInt s


-- 2.5, 2.6

myConcat :: [[a]] -> [a]
myConcat ls = foldr (++) [] ls

-- 2.7

myTakeWhile p [] = []
myTakeWhile p (x:xs) = if p x then x:takeWhile p xs else []

myTakeWhile2 p l = foldr (\a b -> if p a then a:b else []) [] l

-- 2.8, 2.9
--
-- Prelude> :module + Data.List
-- Prelude Data.List> groupBy (\a b -> a `div` 10 == b `div` 10) [1..20]
-- [[1,2,3,4,5,6,7,8,9],[10,11,12,13,14,15,16,17,18,19],[20]]

myGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
myGroupBy p l = foldr f [] l
  where f x (y:ys) = if p x (head y) then (x:y):ys else [x]:y:ys
        f x [] = [[x]]


-- 2.10
-- XXX
-- any, cycle, words, unlines with foldr; wich foldl, which foldl' (and which
-- better?)
