module Main where

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

testSequence :: [Integer]
testSequence = 1 : map next testSequence
  where next x = mod (x + 1234567890) (2^31)

testResults = [ if isPrime x then "1" else "0" | x <- testSequence ]

main :: IO ()
main = do let outputResults = (take 120000 testResults)
          putStr $ concat outputResults

