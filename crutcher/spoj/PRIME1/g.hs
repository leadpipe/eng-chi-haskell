module Main where

import Control.Monad

divides :: Int -> Int -> Bool
divides x y = rem y x == 0

primes = sieve (2 : 3 : [6*k + i | k <- [1..], i <- [-1,1]])

sieve :: [Int] -> [Int]
sieve (x:xs) = x : a ++ sieve (filter (not . divides x) b)
  where (a,b) = span (<x^2) xs


-- Defined only for 2 or higher.
isPrime :: Int -> Bool 
isPrime n = ldp n == n

ldp :: Int -> Int 
ldp n = ldpf primes n 

ldpf :: [Int] -> Int -> Int 
ldpf (p:ps) n
  | p `divides` n = p
  | p^2 > n = n 
  | otherwise = ldpf ps n 


candidates :: Int -> [Int]
candidates n = [6 * k + i | k <- [k0..], i <- [-1,1]]
  where k0 = div (n + 5) 6

candidatesBetween a b
  | a <= 2    = 2 : candidatesBetween 3 b
  | a == 3    = 3 : candidatesBetween 4 b
  | otherwise = takeWhile (<=b) $ candidates a

primesBetween a b = filter isPrime $ candidatesBetween a b

prime1Test :: IO ()
prime1Test
  = do [a,b] <- liftM (map read . words) getLine
       sequence_ $ map print $ pB a b
       putStrLn ""

main :: IO ()
main = do t <- liftM read getLine
          replicateM_ t prime1Test
