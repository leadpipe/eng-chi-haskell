{-# LANGUAGE ScopedTypeVariables, TypeSynonymInstances, EmptyDataDecls #-}
module Rubik.Algebra where

import Data.Monoid (Monoid, mappend, mempty)
import Numeric (showInt)

-- Monoids are useful for expressing the transformations that
-- Rubik-type puzzle pieces undergo.  A monoid is just like a group
-- but without the notion of an inverse.

-- We define some operators to make it easier to work with monoids as
-- group-like types.  We define "one" as a synonym for mempty; the *>
-- operator is our synonym for mappend; and we define ^> to be the
-- result of raising a monoid element to an integral power.

one :: Monoid m => m
one = mempty
(*>) :: Monoid m => m -> m -> m
(*>) = mappend
-- This version of ^> is copied from Data.Monoid.Combinators.replicate
-- in the monoids Hackage package.
(^>) :: (Monoid m, Integral n) => m -> n -> m
base ^> exp
  | exp < 0   = error "Negative exponent"
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


-- | This class lets us easily define monoids corresponding to
-- integers mod any number.
class IntAsType a where
  value :: a -> Int
  showsInt :: a -> Int -> ShowS
  showsInt _ = showInt

newtype Zn n = Zn Int deriving (Eq, Ord)

instance IntAsType n => Show (Zn n) where
  showsPrec _ (Zn x) = showsInt (undefined::n) x

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

instance IntAsType n => Monoid (Zn n) where
  mempty = 0
  mappend = (+)


-- | A "wreath product" is a way to factor a group into two parts, a
-- permutation and some other subgroup.  For example, the corner
-- pieces of a Rubik's cube are permuted by each move, but they are
-- also twisted.
--
-- Our Wreath type is based on Polyonimo permutation code:
--   http://www.polyomino.f2s.com/david/haskell/hs/PermutationGroups.hs.txt
-- We use 0-based indexes, and add a twist monoid to each step.

newtype (Monoid t) => Wreath t = Wreath [WreathMove t]

-- | A WreathMove combines an int index with a twist monoid.
data (Ord t, Monoid t) => WreathMove t = WM Int t deriving (Eq, Ord)

instance (Ord t, Monoid t, Show t) => Show (WreathMove t) where
    showsPrec n (WM i t) = showsPrec n i . showsPrec n t


-- | Look up the move a wreath applies to an index.
getWreathMove :: (Ord t, Monoid t) => Wreath t -> Int -> WreathMove t
getWreathMove (Wreath ms) i = ms `lookup` i
  where lookup (m:ms) 0 = m
        lookup (m:ms) j = lookup ms (j-1)
        lookup [] _ = WM i one -- If the index isn't there, it's not moved

-- | Chain a move through a wreath.
chainWreathMove :: (Ord t, Monoid t) => Wreath t -> WreathMove t -> WreathMove t
chainWreathMove w (WM i t) = let (WM i' t') = getWreathMove w i in WM i' (t *> t')


-- | Wreaths are also Monoids.
instance (Ord t, Monoid t) => Monoid (Wreath t) where
    mempty = Wreath []
    mappend (Wreath ms) w@(Wreath ns) = Wreath (map (chainWreathMove w) ms')
      where ms' = ms ++ [WM i one | i <- [length ms..length ns - 1]]

instance (Ord t, Monoid t) => Eq (Wreath t) where
  Wreath ms == Wreath ns = eqw 0 ms ns
    where eqw i (m:ms) (n:ns) = m == n && eqw (i+1) ms ns
          eqw _ [] [] = True
          eqw i (m:ms) [] = m == WM i one && eqw (i+1) ms []
          eqw i [] (n:ns) = n == WM i one && eqw (i+1) [] ns

instance (Ord t, Monoid t) => Ord (Wreath t) where
    compare w@(Wreath ms) x@(Wreath ns) = if w == x then EQ else compare ms ns
