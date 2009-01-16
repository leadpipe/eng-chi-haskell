
log2 :: Int -> Int
log2 = lg 0
  where
  lg a 1 = a
  lg a n = lg (1+a) (div n 2)

canton :: Int -> (Int, Int)
canton n = (numerator, denominator)
  where
  k = div n 2
  r = n + 1 - div (k*(k+1)) 2
  numerator = if even k then (k+1)-r else 1
  denominator = if even k then r+1 else 1
{-
n = 4
k = div n 2 = 2
r = n + 1 - div (k*(k+1)) 2 = 0
numerator = 3 = (k+1) - r
denominator = r+1

n = 5
k = log2 n = 2
r = n - 2^k = 1
numerator = (k+1) - r = 2
denominator = r+1 = 2

n = 8
k = log2 n = 2
r = n - 2^k = 1
numerator = (k+1) - r = 2
denominator = r+1 = 2

-}

{-
1/1 1/2 1/3 1/4 1/5 ...
2/1 2/2 2/3 2/4
3/1 3/2 3/3
4/1 4/2
5/1

In the above diagram, the first term is 1/1, the second term is 1/2, the
third term is 2/1, the fourth term is 3/1, the fifth term is 2/2, and so
on.
-}
