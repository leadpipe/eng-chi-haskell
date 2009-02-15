import System.Environment (getArgs)

import Data.Char (digitToInt, isDigit)
import Data.List (transpose)


test a b = do
  if a == b
    then putStrLn $ "Ok (" ++ (show a) ++ " == " ++ (show b) ++ ")"
    else error $ (show a) ++ " /= " ++ (show b)



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

-- chokes if infinite lists are used
myAny f l = foldr (\a b -> f a || b) False l  -- works with infinite lists, yay!
--myAny f l = foldr (\a b -> b || f a) False l  -- crap

-- works with infinite lists, but doesn't use foldr either
myAny2 f l = or $ map f l

-- doesn't work with infinite lists, but doesn't stack overflow for them but
-- instead makes your computer unresponsive. is that better? you decide.
myAny3 f l = foldl (\b a -> b || f a) False l


myCycle :: [a] -> [a]
myCycle [] = error "OMG WTF NOOB! myCycle needs a non-empty list."
myCycle l = foldr step [] [1..]  --we need an infinite input for infinite output
  where step a b = l ++ b  -- needs to traverse all of l
                           -- every time it's prepended


myWords :: String -> [String]
myWords s = foldr step [] s
  where
    step :: Char -> [String] -> [String]
    step c []        = if isspace c then []   else [c:[]]
    step c l@([]:ws) = if isspace c then l    else (c:[]):ws
    step c l@(w:ws)  = if isspace c then []:l else (c:w):ws
    isspace c = c == ' '


myUnlines ls = foldr (++) "" $ map (++ "\n") ls


runTests = do
  test True $ myAny even [1..]

  test [1, 2, 1, 2] $ take 4 $ myCycle [1, 2]
  --test [] $ take 4 $ myCycle []  -- how can I test for exceptions?

  test ["OH", "HAI"] $ myWords "OH HAI"
  test ["OH", "HAI"] $ myWords "OH  HAI"
  test [] $ myWords ""
  test [] $ myWords " "
  --test ["a"] $ myWords " a "  -- fails

  test "Oh\nhai\n" $ myUnlines ["Oh", "hai"]
  test "" $ myUnlines []
  test "\n" $ myUnlines [""]

f a | b == 2 = 3
    | otherwise = 4
    where b = 2 * a

--let b = 2*a in
--g a | b == 2 = 3
--    | otherwise = 4

-- questions:
-- Is it possible to handle infinite lists with foldr in finite time?
--   -> Yes, if evaluation stops on the list elements
-- How can I test for exceptions?
--   -> Yes, `handle`, later.
--
-- (Btw: It sucks that inexhaustive patterns are not detected at compile time.
--       I know it's a hard problem, but still :-P)
-- Is it possible to use a guard at the start of an `in` in `let .. in ..`?
--
-- "`undefined` is defined in the prelude

myFoldl :: (a -> b -> a) -> a -> [b] -> a


myFoldl f z xs = (foldr step id xs) z
{-myFoldl f z xs = (foldr (stepasdf f) id xs) z-}
  where
    step :: b -> (a -> a) -> a -> a
    {-step :: b -> (a -> t) -> a -> t-}
    step x g a = g (f a x)

stepasdf f x g a = g (f a x)

-- Homework:
-- * write scale [1, 2, 3] -> [0.3333333, 0.6666, 1] in a cool way
-- * figure out `:t myFoldl.step`





--scale :: [Float] -> [Float]
--scale xs = let (m, xs') = f m xs in xs'
  --where
  --f m' [] = (m', [])
  --f m' (x:xs) = let m'' = max x m'' in (max m'' x, f m' xs)
