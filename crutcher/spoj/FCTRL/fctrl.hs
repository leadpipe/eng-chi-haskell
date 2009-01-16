{-
-- From problem at https://www.spoj.pl/problems/FCTRL/
-- See http://en.wikipedia.org/wiki/Trailing_zeros

* Input

There is a single positive integer T on the first line of input (equal to
about 100000). It stands for the number of numbers to follow. Then there
are T lines, each containing exactly one positive integer number N, 1 <= N
<= 1000000000.

* Output

For every number N, output a single line containing the single non-negative
integer Z(N).

* Example

Sample Input:
6
3
60
100
1024
23456
8735373

Sample Output:
0
14
24
253
5861
2183837
-}

import Control.Monad
import Data.List

-- http://spoj.pl is using an old Haskell with no intercalate
intercalate' :: [a] -> [[a]] -> [a]
intercalate' xs xss = concat (intersperse xs xss)

{-
z :: Int -> Int
z n = sum $ takeWhile (/=0) $ iterate (`div` 5) (n `div` 5)

z :: Int -> Int
z 0 = 0
z n = n' + z n'
  where n' = div n 5
-}

z :: Int -> Int
z n = f 0 n
  where f a 0 = a
        f a n = f (a + n') n'
          where n' = div n 5

slowMain = do t <- (getLine >>= return . read)
              replicateM_ t (getLine >>= print . z . read)

slowMain' = do t <- readLn
               replicateM_ t (readLn >>= print . z)

fastMain = interact g
  where g :: String -> String
        g input = (intercalate' "\n" . map (show . z)) xs
          where ls = lines input
                n = read $ head ls
                xs = map read $ take n $ tail ls

-- fastMain is 3 times faster than slowMain
main = fastMain

