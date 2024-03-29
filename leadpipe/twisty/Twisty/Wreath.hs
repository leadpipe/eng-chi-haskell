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

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Defines the 'Wreath' type, which combines a permutation of a bounded enum
-- with a way to twist each enum element as it's permuted.  This embodies the
-- group theoretic notion of \"wreath product.\"

module Twisty.Wreath
       ( Wreath
       , WreathEntry(..)
       , WreathPermutable
       , WreathTwist
       , getEntry
       , numIndicesMoved
       , getAlteredEntries
       , leavesAllUnaltered
       , leavesAllUnmoved
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

import Data.Array
import Data.Ix (Ix)
import Data.List (foldl')
import qualified Data.Map as Map
import Data.Monoid (Monoid, mappend, mempty)
import Data.Ord (comparing)
import qualified Data.Set as Set


-- | A class for types that can be permuted in a 'Wreath'.  Such types must be
-- bounded, indexable enums with equality testing and order.  And they must have
-- a corresponding type for the ways they can be \"twisted.\"
class (Enum a, Bounded a, Ix a, Eq a, Ord a,
       Group (WreathTwist a), Eq (WreathTwist a), Ord (WreathTwist a))
      => WreathPermutable a where
  -- | The twist group that corresponds to the permuted type.
  type WreathTwist a

-- | A wreath product is a way to factor a group into two parts, a permutation
-- and some other subgroup, which we designate the \"twist\" group.  For
-- example, the corner pieces of a Rubik's cube are permuted by each move, but
-- they are also twisted.
newtype Wreath a = {- WreathPermutable a => -} Wreath (Array a (Entry a))

instance (WreathPermutable a) => Eq (Wreath a) where
  Wreath arr1 == Wreath arr2 = trim arr1 == trim arr2

instance (WreathPermutable a) => Ord (Wreath a) where
  Wreath arr1 `compare` Wreath arr2 = comparing trim arr1 arr2

-- | A WreathEntry combines a value and a twist.  Note that in a Wreath's array
-- the value is the target while the array index is the source, while in a cycle
-- the value is the source.
data {- (WreathPermutable a) => -}
  WreathEntry a = Entry { entryValue :: a
                        , entryTwist :: WreathTwist a
                        }

deriving instance (WreathPermutable a) => Eq (WreathEntry a)
deriving instance (WreathPermutable a) => Ord (WreathEntry a)

-- | A short name for internal use.
type Entry = WreathEntry

instance (WreathPermutable a, Show a, Show (WreathTwist a)) => Show (WreathEntry a) where
    showsPrec n (Entry a t) = showsPrec n a . showsPrec n t


-- | Look up the entry for a value within a wreath.
getEntry :: (WreathPermutable a) => Wreath a -> a -> Entry a
getEntry (Wreath arr) a = if inRange (bounds arr) a then arr!a else Entry a one

-- | Chain an entry through a wreath.
chainEntry :: (WreathPermutable a) => Wreath a -> Entry a -> Entry a
chainEntry w !(Entry a t) = let !(Entry a' t') = getEntry w a in Entry a' (t $* t')

-- | The array we use for empty wreaths.
emptyWreathArray :: (WreathPermutable a) => Array a (Entry a)
emptyWreathArray = listArray (maxBound, pred maxBound) []

-- | Wreaths are Monoids: the identity is the identity permutation, and the
-- append operation is composition of permutations.
instance (WreathPermutable a) => Monoid (Wreath a) where
  mempty = Wreath emptyWreathArray
  mappend w1@(Wreath arr1) w2@(Wreath arr2) = Wreath comp
    where comp = listArray nb [chainEntry w2 (getEntry w1 a) | a <- [fst nb..snd nb]]
          nb = bounds arr1 `union` bounds arr2
          union (b11, b12) (b21, b22) = (min b11 b21, max b12 b22)

-- | Trims the bounds of an array of entries so it consists only of those
-- elements that are either moved or twisted.
trim :: (WreathPermutable a) => Array a (Entry a) -> Array a (Entry a)
trim = trimDown . trimUp
  where trimUp arr = subarray arr (minMoved arr, snd (bounds arr))
        trimDown arr = subarray arr (fst (bounds arr), maxMoved arr)
        subarray arr nb@(n1, n2)
          | nb == bounds arr = arr
          | n1 > n2          = emptyWreathArray
          | otherwise        = listArray nb [arr!a | a <- [n1..n2]]
        -- minMoved returns the upper bound if nothing below it moves.
        minMoved arr = let (b1, b2) = bounds arr in mm b1 b2
          where mm b1 b2 = if b1 >= b2 || arr `moves` b1 then b1
                           else mm (succ b1) b2
        -- maxMoved returns the pred of the lower bound if nothing above the
        -- lower bound moves and the lower bound doesn't move either.
        maxMoved arr = let (b1, b2) = bounds arr in mm b1 b2
          where mm b1 b2 = if b2 < b1 || arr `moves` b2 then b2
                           else mm b1 (pred b2)
        moves arr a = arr!a /= Entry a one

-- | Wreaths are Groups: the inverse is the inverse permutation with all the
-- twists also inverted.
instance (WreathPermutable a) => Group (Wreath a) where
  ginvert (Wreath arr) = Wreath (if Map.null invMap then emptyWreathArray else trim inv)
    where invMap = Map.fromList [(tgt, Entry src (ginvert t)) | (src, Entry tgt t) <- assocs arr]
          inv = listArray (b1, b2) [Map.findWithDefault (Entry a one) a invMap | a <- [b1..b2]]
          (b1, _) = Map.findMin invMap
          (b2, _) = Map.findMax invMap

instance (WreathPermutable a, Show a, Show (WreathTwist a)) => Show (Wreath a) where
    showsPrec _ = fromOptCycles . optShowCycles


-- | Counts the number of indices that are moved by the given wreath.  Ignores
-- the twists: an index that is twisted in place does not add to the count.
numIndicesMoved :: (WreathPermutable a) => Wreath a -> Int
numIndicesMoved (Wreath arr) = foldl' f 0 (assocs arr)
  where f count (src, Entry tgt _)
          | src == tgt = count
          | otherwise  = succ count

-- | Returns the elements that are moved or twisted by this wreath, and how they
-- are moved or twisted.
getAlteredEntries :: (WreathPermutable a) => Wreath a -> [(a, Entry a)]
getAlteredEntries (Wreath arr) = [assoc | assoc@(src, Entry tgt t) <- assocs arr,
                                  src /= tgt || t /= one]

-- | Tells whether this wreath leaves all of the elements of the given list
-- unmoved and untwisted.  The list must be sorted.
leavesAllUnaltered :: (WreathPermutable a) => Wreath a -> [a] -> Bool
leavesAllUnaltered (Wreath arr) = check (assocs arr)
  where check [] _ = True
        check _ [] = True
        check assocsAll@((src, Entry tgt t):assocsTail) asAll@(a:asTail) =
          case compare a src of
            LT -> check assocsAll asTail
            GT -> check assocsTail asAll
            EQ -> src == tgt && t == one && check assocsTail asTail

-- | Tells whether this wreath leaves all of the elements of the given list
-- unmoved, though possibly twisted.  The list must be sorted.
leavesAllUnmoved :: (WreathPermutable a) => Wreath a -> [a] -> Bool
leavesAllUnmoved (Wreath arr) = check (assocs arr)
  where check [] _ = True
        check _ [] = True
        check assocsAll@((src, Entry tgt t):assocsTail) asAll@(a:asTail) =
          case compare a src of
            LT -> check assocsAll asTail
            GT -> check assocsTail asAll
            EQ -> src == tgt && check assocsTail asTail

-- | Converts a wreath into disjoint cycles.
toCycles :: forall a. (WreathPermutable a) => Wreath a -> [[Entry a]]
toCycles (Wreath arr) =
  let (cs, _) = foldl' findCycle ([], Set.empty) (range (bounds arr)) in reverse cs
  where findCycle (cs, seen) src =
          if src `Set.member` seen then (cs, seen)
          else let e@(Entry tgt t) = arr ! src
               in if tgt == src && t == one then (cs, seen)
                  else let (c, srce, seen') = cycle src src e seen in ((srce:c):cs, seen')
        cycle hd src e@(Entry tgt t) seen =
          if tgt == hd then ([], Entry src t, seen)
          else let (c, tgte, seen') = cycle hd tgt (arr!tgt) (Set.insert tgt seen)
               in (Entry src t:c, tgte, seen')


-- | Converts a list of cycles into a wreath.
fromCycles :: (WreathPermutable a) => [[Entry a]] -> Wreath a
fromCycles cs = Wreath (trim arr)
  where arr = base // concatMap fromCycle cs
        base = array (minBound, maxBound) [(a, Entry a one) | a <- [minBound..maxBound]]
        fromCycle es = zip (map entryValue es) (targetEntries es)
        targetEntries es = zipWith Entry (map entryValue (rotate 1 es)) (map entryTwist es)


-- | Optionally shows a wreath as its disjoint cycles; an empty wreath returns
-- Nothing.
optShowCycles :: (WreathPermutable a, Show a, Show (WreathTwist a)) => Wreath a -> OptS
optShowCycles w = showCycles' (toCycles w)
  where showCycles' [] = Nothing
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
