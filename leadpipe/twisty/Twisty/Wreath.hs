{-
Copyright 2011 Google Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, StandaloneDeriving, TypeFamilies, UndecidableInstances #-}

-- | Defines the 'Wreath' type, which combines a permutation of a bounded enum
-- with a way to twist each enum element as it's permuted.  This embodies the
-- group theoretic notion of "wreath product."

module Twisty.Wreath
       ( Wreath
       , WreathEntry(..)
       , WreathPermutable
       , WreathTwist
       , numIndicesMoved
       , toCycles
       , fromCycles
       , optShowCycles
       , showEmptyParens
       , fromOptCycles
       )
where

import Twisty.Group
import Twisty.Lists
import Twisty.Strings

import Data.Array ((//), array, elems)
import Data.Ix (Ix)
import Data.List (elemIndex, sort)
import qualified Data.Map as Map
import Data.Monoid (Monoid, mappend, mempty)


-- | A class for types that can be permuted in a 'Wreath'.  Such types must be
-- bounded, indexable enums with equality testing and order.  And they must have
-- a corresponding type for the ways they can be "twisted."
class (Enum a, Bounded a, Ix a, Eq a, Ord a,
       Group (WreathTwist a), Eq (WreathTwist a), Ord (WreathTwist a))
      => WreathPermutable a where
  type WreathTwist a -- ^ The twist group that corresponds to the permuted type.

-- | A wreath product is a way to factor a group into two parts, a permutation
-- and some other subgroup, which we designate the "twist" group.  For example,
-- the corner pieces of a Rubik's cube are permuted by each move, but they are
-- also twisted.
--
-- This implementation was inspired by the Polyonimo permutation code:
--   http://www.polyomino.f2s.com/david/haskell/hs/PermutationGroups.hs.txt
newtype (WreathPermutable a) => Wreath a = Wreath [Entry a]

-- | A WreathEntry combines the target value and the twist for the source value.
newtype (WreathPermutable a) => WreathEntry a = Entry (a, WreathTwist a)

deriving instance (WreathPermutable a) => Eq (WreathEntry a)
deriving instance (WreathPermutable a) => Ord (WreathEntry a)

-- | A short name for internal use.
type Entry = WreathEntry

instance (WreathPermutable a, Show a, Show (WreathTwist a)) => Show (WreathEntry a) where
    showsPrec n (Entry (a, t)) = showsPrec n a . showsPrec n t


-- | Look up the entry for a value within a wreath.
getEntry :: (WreathPermutable a) => Wreath a -> a -> Entry a
getEntry (Wreath es) a = es `lookup` toIndex a
  where lookup (e:es) 0 = e
        lookup (e:es) i = lookup es (i-1)
        lookup [] _ = Entry (a, one) -- If the index isn't there, it's not moved

-- | Chain an entry through a wreath.
chainEntry :: (WreathPermutable a) => Wreath a -> Entry a -> Entry a
chainEntry w (Entry (a, t)) = let (Entry (a', t')) = getEntry w a in Entry (a', (t $* t'))

-- | Converts a bounded enum value into the list index within the wreath.
toIndex :: forall a. (Enum a, Bounded a) => a -> Int
toIndex a = fromEnum a - fromEnum (minBound::a)

-- | Converts a wreath index into the corresponding bounded enum value.
fromIndex :: forall a. (Enum a, Bounded a) => Int -> a
fromIndex i = toEnum (i + fromEnum (minBound::a))

-- | Wreaths are also Groups.
instance (WreathPermutable a) => Monoid (Wreath a) where
  mempty = Wreath []
  mappend (Wreath es1) w2@(Wreath es2) = Wreath (map (chainEntry w2) es)
    where es = es1 ++ [Entry (fromIndex i, one) | i <- [length es1..length es2 - 1]]

instance (WreathPermutable a) => Group (Wreath a) where
  ginvert (Wreath es) = Wreath (map inv (sort (zip es [0..])))
    where inv (Entry (_,t), i) = Entry (fromIndex i, (ginvert t))

instance (WreathPermutable a) => Eq (Wreath a) where
  Wreath es1 == Wreath es2 = eqw 0 es1 es2
    where eqw i (e1:es1) (e2:es2) = e1 == e2 && eqw (i+1) es1 es2
          eqw _ [] [] = True
          eqw i (e:es) [] = e == Entry (fromIndex i, one) && eqw (i+1) es []
          eqw i [] (e:es) = e == Entry (fromIndex i, one) && eqw (i+1) [] es

instance (WreathPermutable a) => Ord (Wreath a) where
  compare w1@(Wreath es1) w2@(Wreath es2) = if w1 == w2 then EQ else compare es1 es2

instance (WreathPermutable a, Show a, Show (WreathTwist a)) => Show (Wreath a) where
    showsPrec _ = fromOptCycles . optShowCycles


getIndex (Entry (a, _)) = toIndex a

-- | Counts the number of indices that are moved by the given wreath.  Ignores
-- the twists: an index that is twisted in place does not add to the count.
numIndicesMoved :: (WreathPermutable a) => Wreath a -> Int
numIndicesMoved (Wreath list) = f list 0 0
  where f [] _ count = count
        f (e:es) i count = f es (i+1) (if i == getIndex e then count else count+1)

toCycles' :: forall a. (WreathPermutable a) => Wreath a -> [[Entry a]]
toCycles' (Wreath []) = []
toCycles' (Wreath list) =
    let mappings :: Map.Map Int (Entry a)
        mappings = Map.fromDistinctAscList (zip [0..] list)
    in findCycles ([], 0, [], mappings)
    where
      findCycles (cs, i, c, mappings) =
          if Map.null mappings
          then reverse (map invert (c:cs))
          else
              case Map.lookup i mappings of
                Nothing ->
                    let (j, e) = Map.findMin mappings
                    in findCycles (c:cs, getIndex e, [e], Map.delete j mappings)
                Just e -> findCycles (cs, getIndex e, e:c, Map.delete i mappings)
      invert (e:es) = e:(reverse es)


-- | Converts a wreath into disjoint cycles.
toCycles :: (WreathPermutable a) => Wreath a -> [[Entry a]]
toCycles w = filter (not . isUnmoved) (toCycles' w)
    where isUnmoved [(Entry (_, t))] = t == one
          isUnmoved [] = True
          isUnmoved _ = False


-- | Converts a list of cycles into a wreath.
fromCycles :: (WreathPermutable a) => [[Entry a]] -> Wreath a
fromCycles cs = Wreath (elems (array (0, n) [(i, Entry (fromIndex i, one)) | i <- [0..n]] //
                               (concatMap fromCycle cs)))
    where n = maximum (map getIndex (concat cs))
          fromCycle es = zip (map getIndex es) (rotate 1 es)


-- | Optionally shows a wreath as its disjoint cycles; an empty wreath returns
-- Nothing.
optShowCycles :: (WreathPermutable a, Show a, Show (WreathTwist a)) => Wreath a -> OptS
optShowCycles w = showCycles' (toCycles w)
  where showCycles' [] = Nothing
        showCycles' [[]] = Nothing
        showCycles' [c] = toOptS $ showParen True (showEntries c)
        showCycles' (c:cs) = showCycles' [c] $* showCycles' cs
        showEntries [e] = shows e
        showEntries (e:es) = shows e . showChar ' ' . showEntries es

-- | Shows a pair of empty parentheses.
showEmptyParens :: ShowS
showEmptyParens = showString "()"

-- | Converts an OptS of cycles into a ShowS.
fromOptCycles :: OptS -> ShowS
fromOptCycles = fromOptS showEmptyParens
