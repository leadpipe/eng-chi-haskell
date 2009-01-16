
onp :: String -> String
onp xs = fst $ f xs
  where f :: String -> (String, String)
        f [] = ([], [])
        f (x:xs) | x == '(' = (res, tail xs')
                 | elem x "+-*/^" = (res ++ [x], xs')
                 | otherwise = ([x], xs)
          where (res, xs') = f xs

