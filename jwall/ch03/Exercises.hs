--file: ch03/Excercises.hs

myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength(xs)

accumulate (x:xs) = _accumulate x 1 xs
accumulate [] = (0, 0)
_accumulate acc count (x:xs) = _accumulate (acc+x) (count+1) xs
_accumulate acc count [] = (acc, count)

--myMean :: (Fractional a) => [a] -> a
myMean ll = let (amt, len) = accumulate ll
    in amt/len

toPalindrome [] = []
--toPalindrome (x:[]) = [x] -- uncomment to make palindromes of odd size
toPalindrome (x:xs) = x:(toPalindrome xs) ++ [x]
