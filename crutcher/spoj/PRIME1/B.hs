
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



