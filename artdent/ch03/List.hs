data List a = Cons a (List a)
            | Nil
              deriving (Show)

-- Exercise 3.1.1
fromList Nil = []
fromList (Cons x xs) = x:(fromList xs)

-- Exercise 3.2.1
len :: [a] -> Int  -- Exercise 3.2.2
len [] = 0
len (x:xs) = 1 + len xs
len' = foldl accum 0
       where accum count elem = count+1
len'' = foldl accum 0
        where accum = (+) . (const 1)

-- Exercise 3.2.3
--mean :: (Num a) => [a] -> Maybe Float
mean [] = Nothing
mean l = Just $ (fromIntegral (sum l)) / (fromIntegral (length l))
-- TODO: avoid walking the list twice
-- (double accumulator fn with a foldl)

