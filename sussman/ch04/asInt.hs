import Data.Char

-- Original asInt function from book:

loop :: Int -> String -> Int
loop acc (x:xs) = let acc' = acc * 10 + digitToInt x
                  in loop acc' xs
loop acc [] = acc

asInt :: String -> Int
asInt xs = loop 0 xs


-- Rewrite using a fold.
-- Make it handle negative integers.
-- Make it throw error on "", "-", decimal points, overflows.

_myfold :: String -> Int
_myfold xs = foldl step 0 xs
    where step acc x
            | isDigit x = acc * 10 + digitToInt x
            | otherwise = error "non-digit detected"

asInt_fold :: String -> Int
asInt_fold [] = error "illegal empty string"
asInt_fold (x:xs)
   | x == '-' = 0 - (_myfold xs)
   | otherwise = _myfold (x:xs)

-- ### how to detect overflows??

-- ### totally not understanding the 'Either' bit, or what Left and
-- ### Right have to do with anything at all...?

