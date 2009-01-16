addrev line = dropWhile (=='0') $ reverse $ show $ sum $ map read $ words $ reverse line

main = interact g
  where g cs = unlines $ map addrev (take (read n) ts)
          where (n:ts) = lines cs

