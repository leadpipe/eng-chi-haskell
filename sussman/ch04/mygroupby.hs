
-- `group` splits a list into adjacent elements that are equal:
--     group [1,2,2,1,1,1,2,2,2,1] = [[1],[2,2],[1,1,1],[2,2,2],[1]]
--     group "abbcdddeea" =  ["a","bb","c","ddd","ee","a"]

-- variant `groupBy` is the same, but takes an equality function:
--      groupBy (\x y -> (x*y `mod` 3) == 0) [1,2,3,4,5,6,7,8,9]
--                         = [[1],[2,3],[4],[5,6],[7],[8,9]]

-- Goal:  write own implementation using a fold.

myGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
myGroupBy isequal xs = let (acc, blah) = foldl step ([],[]) xs
                       in acc
    where step (bigacc, currentacc) x
            | isequal x (head xs) = (bigacc, currentacc ++ [x])
            | otherwise = (bigacc ++ currentacc, [])

-- ## Hmmm, won't compile, claiming infinite type a = [a]... ?