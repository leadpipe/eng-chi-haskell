{-
SPOJ Problem Set
24. Small factorials
Problem code: FCTRL2

You are asked to calculate factorials of some small positive integers.
Input

An integer t, 1<=t<=100, denoting the number of testcases, followed by t
lines, each containing a single integer n, 1<=n<=100.
Output

For each integer n given at input, display a line with the value of n!
Example
Sample input:

4
1
2
5
3

Sample output:

1
2
120
6
-}

import Control.Monad

facs = 1 : [ f * i | (f, i) <- zip facs [1..]]

fac = (!!) facs

main :: IO ()
main = do t <- liftM read getLine
          replicateM_ t (liftM read getLine >>= print . fac)

