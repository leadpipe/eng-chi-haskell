-- Exercises in creating something isomorphic to Haskell's native list type.

-- Provided by book:  

data List a = Cons a (List a)
            | Nil
              deriving (Show)

fromList (x:xs) = Cons x (fromList xs)
fromList []     = Nil


-- Exercise: "Write the converse of fromList for the List type: a
-- function that takes a List a and generates a [a]."

-- toList :: List -> [a]
toList Nil = []
toList (Cons x xs) = x : (toList xs)
