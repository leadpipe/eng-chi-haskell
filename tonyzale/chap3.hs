data List a = Cons a (List a)
            | Nil
              deriving (Show)

fromList (x:xs) = Cons x (fromList xs)
fromList []     = Nil

fromOurList (Cons x xs) = x:(fromOurList xs)
fromOurList (Nil) = []                           

data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

data MyTree a = Value a (Maybe(MyTree a)) (Maybe(MyTree a))
              deriving (Show)


myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs


meanHelper (x:xs) total count = meanHelper xs (total+x) (count+1)
meanHelper [] total count = total / count

meanList :: [Double] -> Double
meanList x = meanHelper x 0.0 0

