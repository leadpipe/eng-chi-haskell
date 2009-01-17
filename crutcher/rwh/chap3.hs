#!/usr/bin/env runhaskell

import Test.QuickCheck.Batch

import Data.Maybe
import Data.List

options = TestOptions {
  no_of_tests = 200,
  length_of_tests = 1,
  debug_tests = False }

main = do
  tests_1_1
  tests_2_1
  tests_2_3

data List a = Cons a (List a) | Nil
  deriving (Show)

fromList :: [a] -> List a
fromList (x:xs) = Cons x (fromList xs)
fromList [] = Nil

-- Exercise 1.1. Write the converse of fromList for the List type.

toList1 :: List a -> [a]
toList1 Nil = []
toList1 (Cons x xs) = x : toList1 xs

foldListr :: (a -> b -> b) -> b -> List a -> b
foldListr _ z Nil = z
foldListr f z (Cons x xs) = f x $ foldListr f z xs

toList2 = foldListr (:) []

prop_invertsFromList f xs = xs == f (fromList xs)

tests_1_1 = runTests "Exercise 1.1" options [
  run (prop_invertsFromList toList1 :: [Int] -> Bool),
  run (prop_invertsFromList toList2 :: [Int] -> Bool)
  ]


-- Exercise 1.2. Write a tree type that has only one constructor.

data Tree a = Tree a (Maybe (Tree a)) (Maybe (Tree a))
  deriving (Show)

exampleTree = Tree 1 (Just (Tree 2 Nothing Nothing)) Nothing


-- Exercise 2.1. Write a function which computes the number of elements in
-- a list, and compare it to 'length'.

length1 :: [a] -> Int
length1 [] = 0
length1 (_:xs) = 1 + length4 xs

length2 :: [a] -> Int
length2 = foldr (\_ z -> 1 + z) 0

length3 :: [a] -> Int
length3 = foldl' (\z _ -> 1 + z) 0

length4 :: [a] -> Int
length4 = sum . map (const 1)

prop_equalsLength len xs = len xs == length xs

tests_2_1 = runTests "Exercise 2.1" options [
  run (prop_equalsLength length1 :: [Int] -> Bool),
  run (prop_equalsLength length2 :: [Int] -> Bool),
  run (prop_equalsLength length3 :: [Int] -> Bool),
  run (prop_equalsLength length4 :: [Int] -> Bool)
  ]


-- Exercise 2.3. Write a function that computes the mean of a list.

mean1 :: Fractional a => [a] -> a
mean1 [] = 0
mean1 xs = (sum xs) / (fromIntegral $ length xs)

mean2 :: Fractional a => [a] -> a
mean2 [] = 0
mean2 xs = f 0 0 xs
  where f s l [] = s / l
        f s l (x:xs) = f (s + x) (l + 1) xs

mean3 :: Fractional a => [a] -> a
mean3 [] = 0
mean3 xs = f 0 0 xs
  where f s l [] = s / l
        f s l (x:xs) = f (x `seq` s + x) (l `seq` l + 1) xs

prop_computesMean f xs = f xs == mean1 xs

tests_2_3 = runTests "Exercise 2.3" options [
  run (prop_computesMean mean1 :: [Float] -> Bool),
  run (prop_computesMean mean2 :: [Float] -> Bool),
  run (prop_computesMean mean3 :: [Float] -> Bool)
  ]

