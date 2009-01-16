
-- Chapter 3 exercises!

import Data.List

-- 1, 2.  write a function that computes the number of elements in a
-- list.  add a type signature too.

mylen :: [a] -> Integer
mylen l = case l of
            []     -> 0
            (x:xs) -> 1 + mylen xs

-- 3. Write a function that computes the mean of a list, i.e. the sum
-- of all elements in the list divided by its length. (You may need to
-- use the fromIntegral function to convert the length of the list
-- from an integer into a floating point number.)

sumlist l = case l of
              []     -> 0
              (x:xs) -> x + sumlist xs

-- should this all be 1 self-contained function?
meanlist l = (sumlist l) / (fromIntegral (mylen l))


-- 4. Turn a list into a palindrome, i.e. it should read the same both
-- backwards and forwards. For example, given the list [1,2,3], your
-- function should return [1,2,3,3,2,1]

palindrome l = l ++ (reverse l)


-- 5. Write a function that determines whether its input list is a palindrome.

isPalindrome l = (l == reverse l) 


-- 6. Create a function that sorts a list of lists based on the length
-- of each sublist. (You may want to look at the sortBy function from
-- the Data.List module.)

lenCompare a b
  | (length a) < (length b) = LT
  | otherwise = GT

mySort xs = sortBy lenCompare xs


-- 7. a function that joins a list of lists together using a separator value

myintersperse :: a -> [[a]] -> [a]
myintersperse sep l = 
  case l of
     []           -> []
     [a]          -> a
     (x:xs)       -> x ++ sep : myintersperse sep xs


-- 8.  Using the binary tree type that we defined earlier in this
-- chapter, write a function that will determine the height of the
-- tree. The height is the largest number of hops from the root to an
-- Empty. For example, the tree Empty has height zero; Node "x" Empty
-- Empty has height one; Node "x" Empty (Node "y" Empty Empty) Has
-- height two; and so on.

data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

treeHeight :: Tree a -> Integer
treeHeight t =
  case t of
     Empty                  -> 0
     Node a b c             -> 1 + treeHeight b + treeHeight c


-- try evaluating: treeHeight (Node "x" Empty (Node "y" Empty (Node "z" (Node "q"
-- Empty Empty) (Node "r" Empty Empty))))


-- 9. Consider three two-dimensional points a, b, and c. If we look at
-- the angle formed by the line segment from a to b and the line segment
-- from b to c, it either turns left, turns right, or forms a straight
-- line. Define a Direction data type that lets you represent these
-- possibilities.

data Point = Point { xcoord :: Int, ycoord :: Int }
             deriving (Show)

data Direction = LeftTurn | RightTurn | StraightAhead
                 deriving (Show)


-- 10.  Write a function that calculates the turn made by three 2D
-- points and returns a Direction.

calculateTurn :: Point -> Point -> Point -> Direction
calculateTurn a b c = LeftTurn
-- ### finish this... tonyzale says this is a classic videogame
-- ### thing... calculate the dotproduct from a to b and b to
-- ### c... positive/negative dotproduct indicates left/right turn


-- 11.  Define a function that takes a list of 2D points and computes the
-- direction of each successive triple. Given a list of points
-- [a,b,c,d,e], it should begin by computing the turn made by [a,b,c],
-- then the turn made by [b,c,d], then [c,d,e]. Your function should
-- return a list of Direction.

listDirections :: [Point] -> [Direction]
listDirections [] = []
listDirections [a,b] = []
listDirections (x:xs) = (calculateTurn x y z) : listDirections xs
                           where y = head xs
                                 z = head (tail xs)

-- ??? why does this variant not work correctly ??
listDirs (x:y:xs) = (calculateTurn x y z) : listDirs xs
                      where z = head xs
listDirs _ = []


-- 12.  Using the code from the preceding three exercises, implement
-- Graham's scan algorithm for the convex hull of a set of 2D
-- points. You can find good description of what a convex hull. is,
-- and how the Graham scan algorithm should work, on Wikipedia.

--- ### not yet done.
