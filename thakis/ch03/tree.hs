
data Tree a = Tree a (Tree a) (Tree a)
            | Nil
            deriving (Show)

data Tree' a = Tree' a (Maybe (Tree' a)) (Maybe (Tree' a))
             deriving (Show)

--x = Tree' 4 (Just (Tree' 5 Nothing Nothing)) Nothing
