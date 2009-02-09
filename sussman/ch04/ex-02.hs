
import Data.List

-- Write a function splitWith that acts similarly to words, but takes
-- a predicate and a list of any type, and splits its input list on
-- every element for which the predicate returns False.


-- example:  splitWith even [2,4,7,8,10,12,3,5,6,6,4,1]
--           = [[2,4], [8,10,12], [6,6,4]]


splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith pred [] = []
splitWith pred xs
      | null rest = []
      | null chunk = splitWith pred (drop 1 rest)
      | otherwise = chunk : splitWith pred (drop 1 rest)
    where
       tuple = span pred xs
       chunk = fst tuple
       rest = snd tuple

