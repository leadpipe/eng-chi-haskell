import Control.Monad

primecandidates :: [Int]
primecandidates = 2 : 3 : [k + i | k <- [6,12..], i <- [-1,1]] 

primes :: [Int]
primes = sieve primecandidates

divides :: Int -> Int -> Bool
divides x y = mod y x == 0

sieve :: [Int] -> [Int]
sieve (x:xs) = x : (sieve . (filter (not . divides x))) xs

sumDivisors :: Int -> Int
sumDivisors x = g x 1 1 primes
  where
  g :: Int -> Int -> Int -> [Int] -> Int
  g x sd s ps@(p:ps')
    | p*p > x   = sd * s * (1 + x)
    | r == 0    = g x' sd (1 + p*s) ps
    | otherwise = g x (sd * s) 1 ps'
    where
    (x',r) = quotRem x p

sumProperDivisors :: Int -> Int
sumProperDivisors x = sumDivisors x - x

main = interact g
  where g :: String -> String
        g input = (unlines . map (show . sumProperDivisors)) xs
          where ls = lines input
                n = read $ head ls
                xs = map read $ take n $ tail ls

