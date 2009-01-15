-- Exercise 3.1.2
data MTree a = MNode a (Maybe (MTree a)) (Maybe (MTree a))
               deriving (Show)

data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

-- Exercise 3.2.8
height Empty = 0
height (Node val left right) = 1 + (max (height left) (height right))
