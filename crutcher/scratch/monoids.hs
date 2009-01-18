-- See:
-- http://sigfpe.blogspot.com/2009/01/haskell-monoids-and-their-uses.html

import Data.Monoid
import Data.List

newtype Ord a => Max a = Max { getMax :: Maybe a }
  deriving (Show,Ord,Eq)

instance Ord a => Monoid (Max a) where
  mempty = Max Nothing :: Ord a => Max a

  mappend x (Max Nothing) = x
  mappend (Max Nothing) y = y
  mappend (Max (Just x)) (Max (Just y)) = Max (Just (max x y))


newtype Ord a => Min a = Min { getMin :: Maybe a }
  deriving (Show,Ord,Eq)

instance Ord a => Monoid (Min a) where
  mempty = Min Nothing :: Ord a => Min a

  mappend x (Min Nothing) = x
  mappend (Min Nothing) y = y
  mappend (Min (Just x)) (Min (Just y)) = Min (Just (min x y))
  

newtype Ord a => Heap a = Heap { getHeap :: [[a]] }
  deriving (Show)

instance Ord a => Monoid (Heap a) where
  mempty = emptyHeap
  mappend = heapMerge

emptyHeap :: Ord a => Heap a
emptyHeap = Heap []

heapEmpty :: Ord a => Heap a -> Bool
heapEmpty (Heap []) = True 
heapEmpty _ = False

makeHeap :: Ord a => [a] -> Heap a
makeHeap xs = Heap [sort xs]

heapMerge :: Ord a => Heap a -> Heap a -> Heap a
heapMerge a b = Heap (f (getHeap a) (getHeap b))
  where
  f :: Ord a => [[a]] -> [[a]] -> [[a]]
  f a [] = a
  f [] b = b
  f a@(xs:xss) b@(ys:yss)
    | head xs <= head ys = xs : f xss b
    | otherwise =          ys : f a yss

-- only defined on non-empty heaps
heapPeek :: Ord a => Heap a -> a
heapPeek (Heap ((x:_):_)) = x

heapPop :: Ord a => Heap a -> (a, Heap a)
heapPop (Heap ((x:xs):xss))
  | null xs = (x, Heap xss)
  | otherwise = (x, heapMerge (Heap [xs]) (Heap (xss)))

heapPush :: Ord a => a -> Heap a -> Heap a
heapPush x = mappend (makeHeap [x])

