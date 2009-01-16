{-
2. Prime Generator
Problem code: PRIME1

Peter wants to generate some prime numbers for his cryptosystem. Help him!
Your task is to generate all prime numbers between two given numbers!

Input

The input begins with the number t of test cases in a single line (t<=10).
In each of the next t lines there are two numbers m and n (1 <= m <= n <=
1000000000, n-m<=100000) separated by a space.

Output

For every test case print all prime numbers p such that m <= p <= n, one
number per line, test cases separated by an empty line.

Example

Input:
2
1 10
3 5

Output:
2
3
5
7

3
5
-}

import Control.Monad

sieve :: Ord a => [a] -> [a] -> [a]
sieve _ [] = []
sieve [] xs = xs
sieve fs@(f:fs') xs@(x:xs') 
  | x < f     = x : sieve fs xs'
  | x == f    = sieve fs' xs'
  | otherwise = sieve fs' xs

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge xs@(x:xs') ys@(y:ys')
  | x < y     = x : merge xs' ys
  | x == y    = x : merge xs' ys'
  | otherwise = y : merge xs ys'

mergeMany :: Ord a => [[a]] -> [a]
mergeMany (x:[]) = x
mergeMany (x:xs) = merge x (mergeMany xs)

primes :: [Integer] 
primes = 2 : filter isPrime [3,5..] 

isPrime :: Integer -> Bool 
isPrime 1 = False
isPrime n = ldp n == n

ldp :: Integer -> Integer 
ldp n = ldpf primes n 

ldpf :: [Integer] -> Integer -> Integer 
ldpf (p:ps) n
  | rem n p == 0 = p 
  | p^2 > n = n 
  | otherwise = ldpf ps n 

genMults f a b = [k, k + f .. b]
  where a' = f * (div (a + (f - 1)) f)
        k = if f /= a' then a' else a' + f

primesBetween a b = filter isPrime (sieve (mergeMany css) xs)
  where a' = if a == 1 then 2 else a
        css = [ genMults p a b | p <- take 30 (tail primes) ]
        xs = if a <= 2
               then 2 : [3,5..b]
               else if even a then [a+1,a+3..b] else [a,a+1..b]

prime1Test :: IO ()
prime1Test
  = do [a,b] <- liftM (map read . words) getLine
       sequence $ map print $ primesBetween a b
       putStrLn ""

main :: IO ()
main = do t <- liftM read getLine
          replicateM_ t prime1Test

