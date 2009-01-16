
-- Book gives us this constructor and instantiation:

data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

simpleTree = Node "parent" (Node "left child" Empty Empty)
                           (Node "right child" Empty Empty)

-- Exercise: define a tree type with only one constructor, using Maybe
-- instead of Empty to refer to the children.

data MyTree a = Maybe (a (MyTree a) (MyTree a))
                deriving (Show)

newTree = Just ("parent" (Just ("left" Nothing Nothing))
                         (Just ("right" Nothing Nothing)))
