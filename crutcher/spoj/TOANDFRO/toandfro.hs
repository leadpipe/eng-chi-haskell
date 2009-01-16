import Data.List

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = let (as,xs') = splitAt n xs in as : chunk n xs'

reverseOdds :: [[a]] -> [[a]]
reverseOdds xs = odd xs
  where odd [] = []
        odd (x:xs) = x : even xs
        even [] = []
        even (x:xs) = reverse x : odd xs

decode n = concat . transpose . reverseOdds . (chunk n)

main = do
  n <- readLn
  if n == 0 then return ()
    else do cs <- getLine
            putStrLn $ decode n cs
            main

