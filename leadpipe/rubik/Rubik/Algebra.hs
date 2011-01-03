{-# LANGUAGE ScopedTypeVariables, TypeSynonymInstances, EmptyDataDecls #-}
module Rubik.Algebra where

import Data.List (sort)
import Data.Monoid (Monoid, mappend, mempty, Dual(..), Sum(..))
import GHC.Enum (boundedEnumFrom, boundedEnumFromThen)
import Numeric (showInt)

-- | We extend Monoid to make a group class, by adding an inverse operator.
class Monoid a => Group a where
  ginvert :: a -> a
  -- ^ Returns the inverse of the given group element, ie the element that
  -- yields mempty when mappend'ed to the original element.


-- We define some operators to make it easier to work with monoids and groups.
-- We define "one" as a synonym for mempty; the *> operator is our synonym for
-- mappend; and we define ^> to be the result of raising a group element to an
-- integral power.

one :: Monoid m => m
one = mempty
(*>) :: Monoid m => m -> m -> m
(*>) = mappend
-- This version of ^> is copied from Data.Group.Combinators.replicate
-- in the monoids Hackage package.
(^>) :: (Group g, Integral n) => g -> n -> g
base ^> exp
  | exp < 0   = ginvert base ^> negate exp
  | exp == 0  = one
  | otherwise = f base exp
    where
      f base exp
        | exp == 1  = base
        | even exp  = f (base *> base) (exp `quot` 2)
        | otherwise = g (base *> base) (exp `quot` 2) base
      g base exp acc
        | exp == 1  = base *> acc
        | even exp  = g (base *> base) (exp `quot` 2) acc
        | otherwise = g (base *> base) (exp `quot` 2) (base *> acc)


-- Some Group defs corresponding to (some of) the basic Monoid defs:
instance (Group b) => Group (a -> b) where
  ginvert f x = ginvert (f x)

instance Group () where
  ginvert () = ()

instance (Group a, Group b) => Group (a, b) where
  ginvert (a, b) = (ginvert a, ginvert b)

instance (Group a, Group b, Group c) => Group (a, b, c) where
  ginvert (a, b, c) = (ginvert a, ginvert b, ginvert c)

instance (Group a, Group b, Group c, Group d) => Group (a, b, c, d) where
  ginvert (a, b, c, d) = (ginvert a, ginvert b, ginvert c, ginvert d)

instance (Group a, Group b, Group c, Group d, Group e) => Group (a, b, c, d, e) where
  ginvert (a, b, c, d, e) = (ginvert a, ginvert b, ginvert c, ginvert d, ginvert e)

instance (Group a) => Group (Dual a) where
  ginvert (Dual x) = Dual (ginvert x)

instance (Num a) => Group (Sum a) where
  ginvert (Sum x) = Sum (negate x)


-- | This class lets us easily define groups corresponding to
-- integers mod any number.
class IntAsType a where
  value :: a -> Int
  showsInt :: a -> Int -> ShowS
  showsInt _ = shows
  readsInt :: a -> ReadS Int
  readsInt _ = reads

newtype Zn n = Zn Int deriving (Eq, Ord)

instance IntAsType n => Show (Zn n) where
  showsPrec _ (Zn x) = showsInt (undefined::n) x

instance IntAsType n => Read (Zn n) where
  readsPrec _ s = map f $ readsInt (undefined::n) s
    where f (x, s) = (Zn x, s)

instance IntAsType n => Num (Zn n) where
  Zn x + Zn y = Zn $ (x+y) `mod` value (undefined :: n)
  negate (Zn 0) = 0
  negate (Zn x) = Zn $ value (undefined :: n) - x
  Zn x * Zn y = Zn $ (x*y) `mod` value (undefined :: n)
  fromInteger n = Zn $ fromInteger n `mod` value (undefined :: n)
  -- | Not well defined for these types.
  abs = id
  -- | Not well defined for these types.
  signum (Zn 0) = 0
  signum _ = 1

instance IntAsType n => Real (Zn n) where
  toRational (Zn n) = toRational n

instance IntAsType n => Integral (Zn n) where
  (Zn n1) `quotRem` (Zn n2) = (Zn q, Zn r)
    where (q, r) = n1 `quotRem` n2
  toInteger (Zn n) = toInteger n

instance IntAsType n => Enum (Zn n) where
  toEnum i = Zn $ i `mod` value (undefined::n)
  fromEnum (Zn n) = n
  enumFrom = boundedEnumFrom
  enumFromThen = boundedEnumFromThen

instance IntAsType n => Bounded (Zn n) where
  minBound = 0
  maxBound = Zn $ value (undefined::n) - 1

instance IntAsType n => Monoid (Zn n) where
  mempty = 0
  mappend = (+)

instance IntAsType n => Group (Zn n) where
  ginvert = negate


-- | A "wreath product" is a way to factor a group into two parts, a
-- permutation and some other subgroup.  For example, the corner
-- pieces of a Rubik's cube are permuted by each move, but they are
-- also twisted.
--
-- Our Wreath type is based on Polyonimo permutation code:
--   http://www.polyomino.f2s.com/david/haskell/hs/PermutationGroups.hs.txt
-- We use 0-based indexes, and add a twist monoid to each step.

newtype (Group t) => Wreath t = Wreath [WreathMove t]

-- | A WreathMove combines an int index with a twist group.
newtype (Ord t, Group t) => WreathMove t = WM (Int, t) deriving (Eq, Ord)

instance (Ord t, Group t, Show t) => Show (WreathMove t) where
    showsPrec n (WM (i,t)) = showsPrec n i . showsPrec n t


-- | Look up the move a wreath applies to an index.
getWreathMove :: (Ord t, Group t) => Wreath t -> Int -> WreathMove t
getWreathMove (Wreath ms) i = ms `lookup` i
  where lookup (m:ms) 0 = m
        lookup (m:ms) j = lookup ms (j-1)
        lookup [] _ = WM (i,one) -- If the index isn't there, it's not moved

-- | Chain a move through a wreath.
chainWreathMove :: (Ord t, Group t) => Wreath t -> WreathMove t -> WreathMove t
chainWreathMove w (WM (i,t)) = let (WM (i',t')) = getWreathMove w i in WM (i', (t *> t'))


-- | Wreaths are also Groups.
instance (Ord t, Group t) => Monoid (Wreath t) where
    mempty = Wreath []
    mappend (Wreath ms) w@(Wreath ns) = Wreath (map (chainWreathMove w) ms')
      where ms' = ms ++ [WM (i, one) | i <- [length ms..length ns - 1]]

instance (Ord t, Group t) => Group (Wreath t) where
    ginvert (Wreath ms) = Wreath (map inv (sort (zip ms [0..])))
      where inv (WM (_,t), i) = WM (i, (ginvert t))

instance (Ord t, Group t) => Eq (Wreath t) where
  Wreath ms == Wreath ns = eqw 0 ms ns
    where eqw i (m:ms) (n:ns) = m == n && eqw (i+1) ms ns
          eqw _ [] [] = True
          eqw i (m:ms) [] = m == WM (i,one) && eqw (i+1) ms []
          eqw i [] (n:ns) = n == WM (i,one) && eqw (i+1) [] ns

instance (Ord t, Group t) => Ord (Wreath t) where
    compare w@(Wreath ms) x@(Wreath ns) = if w == x then EQ else compare ms ns
