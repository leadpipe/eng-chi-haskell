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

data PalinType = Odd | Even
    deriving (Show)

evenPalindrome ll = toPalindrome Even ll 
oddPalindrome ll = toPalindrome Odd ll 

toPalindrome palintype [] = []
toPalindrome Odd (x:[]) = [x]
toPalindrome palintype (x:xs) = x:(toPalindrome palintype xs) ++ [x]

isPalindrome ll = ll == (reverse ll)
