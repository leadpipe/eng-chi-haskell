
-- Book gives us this constructor and instantiation:

data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

simpleTree = Node "parent" (Node "left child" Empty Empty)
                           (Node "right child" Empty Empty)

-- Exercise: define a tree type with only one constructor, using Maybe
-- instead of Empty to refer to the children.

data myTree a = Node a (Maybe (myTree a)) (Maybe (myTree a))
                deriving (Show)

newTree = Node "parent" Just (Node "left" Nothing Nothing)
                        Just (Node "right" Nothing Nothing)

