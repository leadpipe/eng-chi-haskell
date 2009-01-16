module Main where

{-

1. Life, the Universe, and Everything
Problem code: TEST

Your program is to use the brute-force approach in order to find the Answer
to Life, the Universe, and Everything. More precisely... rewrite small
numbers from input to output. Stop processing input after reading in the
number 42. All numbers at input are integers of one or two digits.

Example

Input:
1
2
88
42
99

Output:
1
2
88

main = do n <- getLine >>= return . read
          if n == 42
            then return ()
            else print n >> main
-}

main :: IO ()
main = do num <- getLine
          if (read num) /= 42
            then do putStrLn num
                    main
            else return ()

{-
I think it is possible to solve this without recursing on 'main', using
some form of fmap. I want to construct the infinate list of numbers
resulting from reading stdin, and then takeWhile that list, and then print
it. This is of course tricky, but I think monads will let me do it.
-}
