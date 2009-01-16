
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
myintersperse sep l
   | l == []        = []
   | length l == 1  = head l
   | otherwise      = head l ++ sep ++ myintersperse sep (tail l)
