module Main where

import List

primes = 2 : filter isPrime [3,5..] 

isPrime 1 = False
isPrime n = ldp n == n

ldp n = ldpf primes n 

ldpf (p:ps) n
  | rem n p == 0 = p 
  | p^2 > n = n 
  | otherwise = ldpf ps n 

primesBetween :: Int -> Int -> [Int]
primesBetween 0 n = primesBetween 2 n
primesBetween 1 n = primesBetween 2 n
primesBetween m n = f ps [m..n]
  where ps = takeWhile (\x -> x^2 <= m) primes
        f [] xs = xs
        f (p:ps) xs = f ps $ filter (\x -> p >= x || rem x p /= 0) xs

prime1Test :: IO ()
prime1Test = do range <- getLine >>= return . (map read . words) 
                let ps = primesBetween (range !! 0) (range !! 1)
                sequence $ map print ps
                putStrLn ""
                return ()
  where intercalate xs xss = concat (intersperse xs xss)

main :: IO ()
main = do numTests <- getLine >>= return . read
          sequence $ replicate numTests prime1Test
          return ()



