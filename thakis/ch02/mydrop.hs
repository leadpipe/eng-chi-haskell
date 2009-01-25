mydrop n xs =
  if n <= 0 || null xs
  then xs
  else mydrop (n - 1) (tail xs)

lastButOne :: [a] -> a
lastButOne (a:b:[]) = a
lastButOne (x:xs) = lastButOne xs
