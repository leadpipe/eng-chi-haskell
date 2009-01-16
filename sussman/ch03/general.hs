
-- Chapter 3 exercises!

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

isPalindrome l = case l of
                   []       -> True
                   [x]      -> False
                   (x:xs)   -> (x == (last xs)) && 
                               (isPalindrome (take (n-1) xs))
                                  where n = length xs


