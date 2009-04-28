import Control.Applicative

data List a = Empty | Cons { lHead :: a, lTail :: List a }
  deriving (Show)

lNull :: List a -> Bool
lNull Empty = True
lNull _ = False

infixr 5 +++

(+++) :: List a -> List a -> List a
Empty +++ ys = ys
xs +++ Empty = xs
(Cons x xs) +++ ys = Cons x (xs +++ ys)

lConcat :: List (List a) -> List a
lConcat Empty = Empty
lConcat (Cons xs xss) = xs +++ lConcat xss

lIntersperse :: a -> List a -> List a
lIntersperse _ Empty = Empty
lIntersperse x ys@(Cons y ys')
  | lNull ys' = ys
  | otherwise = Cons y (Cons x (lIntersperse x ys'))

lIntercalate :: List a -> List (List a) -> List a
lIntercalate as bss = lConcat (lIntersperse as bss)

instance Eq a => Eq (List a) where
  (==) Empty Empty = True
  (==) (Cons x xs) (Cons y ys)
    | x == y    = xs == ys
    | otherwise = False
  (==) _ _ = False

instance Ord a => Ord (List a) where
  compare Empty Empty = EQ
  compare Empty _ = LT
  compare _ Empty = GT
  compare (Cons x xs) (Cons y ys)
    = case compare x y of
        EQ -> compare xs ys
        ord  -> ord

instance Functor List where
  fmap f Empty = Empty
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

{- Functor Law Proofs:
 -
 - Functor Identity: fmap id = id
 -
 - Base case:
 - id Empty = Empty
 - fmap id Empty = Empty
 - 
 - Inductive case:
 - id (Cons x xs) = Cons (id x) (id xs)
 - fmap id (Cons x xs) = Cons (id x) (fmap id xs)
 -
 - Functor Composition: fmap (g . h) = fmap g . fmap h
 - 
 - Base case:
 - fmap (g . h) Empty = Empty
 - (fmap g . fmap h) Empty
 -   = fmap g (fmap h Empty)
 -   = fmap g Empty
 -   = Empty
 -
 - Inductive case:
 - fmap (g . h) (Cons x xs) = Cons ((g . h) x) (fmap (g . h) xs)
 - (fmap g . fmap h) (Cons x xs)
 -   = fmap g (Cons (h x) (fmap h xs))
 -   = Cons (g (h x)) (fmap g (fmap h xs))
 -   = Cons ((g . h) x) ((fmap g . fmap h) xs)
 -}

instance Applicative List where
  pure x = Cons x Empty

  Empty <*> _ = Empty
  _ <*> Empty = Empty
  (Cons f fs) <*> xs = (fmap f xs) +++ (fs <*> xs)

{- Applicative Laws:
 -
 - Common Law: fmap g x = pure g <*> x
 -
 - Base case:
 - fmap g Empty
 -   = Empty
 -   = pure g <*> Empty
 -
 - Inductive case:
 - pure g <*> xs
 -   = (Cons g Empty) <*> xs
 -   = (fmap g xs) +++ (Empty <*> xs)
 -   = (fmap g xs) +++ Empty
 -   = fmap g xs
 -
 - Applicative Identity: pure id <*> u = u
 - pure id <*> u = fmap id x = x
 -
 - Applicative Composition: pure '.' <*> u <*> v <*> w = u <*> (v <*> w)
 - pure '.' <*> u <*> v <*> w
 -   = fmap '.' u <*> v <*> w
 -   TODO
 -
 - Applicative Homomorphism: pure f <*> pure g = pure (f g)
 - pure f <*> pure g
 -   = (Cons f Empty) <*> (Cons g Empty)
 -   = (fmap f (Cons g Empty)) +++ (Empty <*> (Cons g Empty))
 -   = fmap f (Cons g Empty) +++ Empty
 -   = fmap f (Cons g Empty)
 -   = Cons (f g) Empty
 -   = pure (f g)
 -
 - Applicative Interchange: u <*> pure x = pure (\f -> f x) <*> u
 - Base case: u = Empty
 - pure (\f -> f x) <*> Empty
 -   = Empty
 -   = Empty <*> pure x
 -
 - Inductive case:
 - pure (\f -> f x) <*> ys
 -   = (Cons (\f -> f x) Empty) <*> ys
 -   = (fmap (\f -> f x) ys) +++ (Empty <*> ys)
 -   = (fmap (\f -> f x) ys) +++ Empty
 -   = fmap (\f -> f x) ys
 -   = pure (\f -> f x) <*> ys
 -
 - Inductive case; u = (Cons y ys)
 - pure (\f -> f x) <*> (Cons y ys)
 -   = (Cons (\f -> f x) Empty) <*> (Cons y ys)
 -   = fmap (\f -> f x) (Cons y ys)
 -   = Cons (y x) (fmap (\f -> f x) ys)
 -   = (Cons (y x) Empty) +++ fmap (\f -> f x) ys
 -   = (Cons (y x) Empty) +++ (pure (\f -> f x) <*> ys)
 -   = (Cons (y x) Empty) +++ (ys <*> pure x) -- Via Induction
 -   = (Cons (y x) Empty) +++ (ys <*> (Cons x Empty))
 -   = (Cons y ys) <*> (Cons x Empty)
 -   = (Cons y ys) <*> pure x
 -}

instance Monad List where
  return = pure

  -- join = lConcat

  -- m >>= f = join (fmap f m)
  m >>= f = lConcat (fmap f m)

  fail _ = Empty

{- Monad Laws
 - Law 1: return a >>= k  ==  k a
 - return a >>= k
 -   = pure a >>= k
 -   = lConcat (fmap k (pure a))
 -   = lConcat (fmap k (Cons a Empty))
 -   = lConcat (Cons (k a) Empty)
 -   = (k a) +++ lConcat Empty 
 -   = (k a) +++ Empty
 -   = k a
 -
 - Law 2: m >>= return  ==  m
 - m >>= return
 -   = lConcat (fmap return m)
 -   = lConcat (fmap pure m)
 -   = m
 -
 - Law 3: m >>= (\x -> k x >>= h)  ==  (m >>= k) >>= h
 - TODO
-}


