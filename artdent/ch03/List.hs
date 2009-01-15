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

-- Exercise 3.2.4
palin [] = []
palin (x:xs) = x:(palin xs) ++ [x]

-- Exercise 3.2.5
isPalin :: [a] -> Bool = undefined

-- Exercise 3.2.6
sortListsByLen :: [[a]] -> Int = undefined

-- Exercise 3.2.7
intersperse val = foldl accum []
    where accum [] ll = ll
          accum ll ll' = ll ++ [val] ++ ll'
