-- Exercise 3.1.2

data Tree a = Node a (Maybe (Tree a)) (Maybe (Tree a))
              deriving (Show)
