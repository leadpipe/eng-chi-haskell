
-- Write your own definition of the standard takeWhile function, first
-- using explicit recursion, then foldr.

-- (Definition: takes elements from a list, up until some element fails
-- the predicate.)

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile pred xs = looper xs
   where looper (y:ys)
             | pred y = y : (looper ys)
             | otherwise = []
         looper [] = []


myTakeWhile_fold :: (a -> Bool) -> [a] -> [a]
myTakeWhile_fold pred xs = foldr step [] xs
    where step y ys
             | pred y = y : ys
             | otherwise = []