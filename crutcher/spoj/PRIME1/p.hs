module Main where

import Control.Monad
import Data.List

divides :: Int -> Int -> Bool
divides x y = rem y x == 0

primes :: [Int] 
primes = 2 : 3 : 5 : filter isPrime [k + i | k <- [6,12..], i <- [1,5]]

ldp :: Int -> Int 
ldp n = ldpf primes n 

ldpf :: [Int] -> Int -> Int 
ldpf (p:ps) n
  | p*p > n = n 
  | rem n p == 0 = p
  | otherwise = ldpf ps n 

-- Defined only for 2 or higher.
isPrime :: Int -> Bool 
isPrime n = ldp n == n

primeCandidates :: Int -> [Int]
primeCandidates n
  | n <= 2    = 2 : primeCandidates 3
  | n == 3    = 3 : primeCandidates 5
  | otherwise = dropWhile (<n) [k + i | k <- [k0,k0+6..], i <- [-1,1]]
  where k0 = 6 * (div n 6)

candidatesBetween :: Int -> Int -> [Int]
candidatesBetween a b = takeWhile(<=b) $ primeCandidates a

primesBetween :: Int -> Int -> [Int]
primesBetween a b = filter isPrime $ candidatesBetween a b

{-
prime1Test :: IO ()
prime1Test
  = do [a,b] <- liftM (map read . words) getLine
       sequence_ $ map print $ primesBetween a b
       putStrLn ""

main :: IO ()
main = do t <- liftM read getLine
          replicateM_ t prime1Test
-}

main = interact g
  where
  g input = unlines $ map f $ take (read t) ts
    where (t:ts) = lines input
  f line = unlines $ map show $ (primesBetween a b)
    where [a,b] = map read (words line)

