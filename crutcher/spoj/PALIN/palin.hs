
a = "99"

palin' :: String -> String
palin' xs
  | ras > bss = as ++ extra ++ ras
  | otherwise = cs ++ (if isOdd then tail rcs else rcs)
  where
   (as,bs) = splitAt (div (length xs) 2) xs
   bss = if isOdd then tail bs else bs
   extra = if isOdd then [head bs] else []
   cs = reverse $ plus1 $ extra ++ ras
   plus1 [] = []
   plus1 (x:xs)
     | x < '9' = (show ( 1 + read [x] :: Int)) ++ xs
     | otherwise = "0" ++ (plus1 xs)
   ras = reverse as
   rcs = reverse cs
   isOdd = odd $ length xs

palin :: String -> String
palin cs 
  | zs > ys   = xs ++ zs
  | otherwise = xs' ++ zs'
  where
  n = length cs
  e = even n
  (as, bs) = splitAt (div n 2) cs
  xs = if e then as else as ++ [head bs]
  xs' = show $ 1 + read xs
  ys = if e then bs else tail bs
  zs = reverse as
  zs' = drop (if e then 0 else 1) $ reverse xs'

main = interact g
  where g :: String -> String
        g input = unlines $ map palin $ take (read n) ts
          where (n:ts) = lines input


