-- ch03/exercises.hs
-- by pdarga@gmail.com (Paul Darga)

import Data.List (sortBy)
import Data.Ord (comparing)

-- from the text

data List a = Cons a (List a) | Nil
	deriving Show

data Tree a = Node a (Tree a) (Tree a) | Empty
	deriving Show

data Direction = TurnsLeft | TurnsRight | GoesStraight

-- Exercise group 1, question 2:
-- Define a tree type that has only one constructor,
-- like our Java example. Instead of the Empty constructor,
-- use the Maybe type to refer to a node's children.

-- Non-isomorphic solution: can't represent empty tree

data TreeA a = NodeA a (Maybe (TreeA a)) (Maybe (TreeA a))

-- Isomorphic solution: move the maybe one level up

type TreeB a = Maybe (TreeB' a)
data TreeB' a = NodeB a (TreeB a) (TreeB a)

-- Exercise group 1, question 1:
-- Write the converse of fromList for the List type:
-- a function that takes a List a and generates a [a].

fromList :: List a -> [a]
fromList = foldList (:) [] where
	foldList _ x Nil = x
	foldList f x (Cons y ys) = f y (foldList f x ys)

-- Exercise group 2, question 2:
-- Add a type signature for your function to your source file.
-- To test it, load the source file into ghci again.

length' :: [a] -> Int

-- Exercise group 2, question 1:
-- Write a function that computes the number of elements
-- in a list. To test it, ensure that it gives the same
-- answers as the standard length function.

length' = sum . map (const 1)

-- Exercise group 2, question 3:
-- Write a function that computes the mean of a list, i.e.
-- the sum of all elements in the list divided by its
-- length. (You may need to use the fromIntegral function
-- to convert the length of the list from an integer into
-- a floating point number.)

mean :: [Int] -> Double
mean xs = fromIntegral (sum xs) /  fromIntegral (length xs)

-- Exercise group 2, question 4:
-- Turn a list into a palindrome, i.e. it should read
-- the same both backwards and forwards. For example,
-- given the list [1,2,3], your function should return
-- [1,2,3,3,2,1].

palindromate :: [a] -> [a]
palindromate xs = xs ++ reverse xs

-- Exercise group 2, question 5:
-- Write a function that determines whether its input list
-- is a palindrome.

ispalindrome :: (Eq a) => [a] -> Bool
ispalindrome xs = xs == reverse xs

-- Exercise group 2, question 6:
-- Create a function that sorts a list of lists based
-- on the length of each sublist. (You may want to look
-- at the sortBy function from the Data.List module.)

sortByLength = reverse . sortBy (comparing length)

-- Exercise group 2, question 7:
-- Define a function that joins a list of lists together
-- using a separator value.

intersperse :: a -> [[a]] -> [a]
intersperse _ [] = []
intersperse sep (x:xs) = x ++ concatMap (sep:) xs

-- Exercise group 2, question 8:
-- Using the binary tree type that we defined earlier in
-- this chapter, write a function that will determine the
-- height of the tree. The height is the largest number of
-- hops from the root to an Empty. For example, the tree
-- Empty has height zero; Node "x" Empty Empty has height
-- one; Node "x" Empty (Node "y" Empty Empty) has height
-- two; and so on.

-- I'm broken
-- height :: Tree a -> Int
-- height = foldTree ((+1) . (const . max)) 0 where
-- 	foldTree _ z Empty = z
-- 	foldTree f z (Node w x y) = f w (foldTree f z x) (foldTree f z y)


