
-- Write your own definition of concat using foldr

myconcat :: [[a]] -> [a]
myconcat lists = foldr (++) [] lists

