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

-- | Defines the 'FaceTwist' type, which describes the possible moves of a
-- twisty puzzle.  Also defines 'CumulativeTwists', a way to characterize
-- algorithms that is useful for finding good algorithms.
module Twisty.FaceTwist
       ( FaceTwist(..)
       , CumulativeTwists
       , emptyTwists
       , updateTwists
       )
where

import Twisty.Group
import qualified Twisty.Memo as Memo
import Twisty.Polyhedron
import Twisty.Puzzle

import Data.Ix (Ix)
import Data.Map (Map)
import qualified Data.Map as Map
--import Data.Maybe (listToMaybe, maybeToList)

-- | Each value has a face to twist, the number of layers to twist, and how far
-- to twist.
data (PolyFace face, Ord depth, Group twist, Ord twist) =>
     FaceTwist face depth twist = FaceTwist face depth twist deriving (Eq, Bounded, Ix)

instance (PolyFace f, Ord d, Group t, Ord t) => Ord (FaceTwist f d t) where
  -- | Larger depths compare as less, to match the normal way of twisting:
  -- deeper moves come before shallower ones.
  compare (FaceTwist f1 d1 t1) (FaceTwist f2 d2 t2) = compare (f1, d2, t1) (f2, d1, t2)

instance (PolyFace f, Bounded d, Ord d, Ix d, Group t, Bounded t, Ord t, Ix t) =>
         PuzzleMove (FaceTwist f d t) where
  undoMove (FaceTwist f d t) = FaceTwist f d (ginvert t)

  joinMoves = table (Memo.array jm)
    where table memo move1 move2 = memo (move1, move2)
          jm (m1@(FaceTwist f1 d1 t1), m2@(FaceTwist f2 d2 t2))
            | f1 == f2 && d1 == d2   = let t = t1 $* t2 in
                                       if t == one then [] else [FaceTwist f1 d1 t]
            | f1 `neighbors` f2      = [m1, m2]
            | otherwise              = [max m1 m2, min m1 m2]

  isTrivialMove (FaceTwist _ _ t) = t == one

-- These default defs cause overlapping instances:
-- instance (PolyFace f, Show f, Ord d, Show d, Group t, Ord t, Show t) =>
--          Show (FaceTwist f d t) where
--   showsPrec _ (FaceTwist f d t) = shows f . shows d . shows t

-- instance (PolyFace f, Read f, Ord d, Read d, Group t, Ord t, Read t) =>
--          Read (FaceTwist f d t) where
--   readsPrec _ "" = []
--   readsPrec _ (c:s) = maybeToList $ do
--     f <- nameToMaybeFace c
--     (d, s') <- listToMaybe (reads s)
--     (t, s'') <- listToMaybe (reads s')
--     return (FaceTwist f d t, s'')


-- | Adds up all the twists associated with each (face, depth) pair.
type CumulativeTwists f d t = Map (f, d) t

emptyTwists :: CumulativeTwists f d t
emptyTwists = Map.empty

updateTwists :: (PolyFace f, Ord d, Group t, Ord t) =>
                CumulativeTwists f d t -> FaceTwist f d t -> CumulativeTwists f d t
updateTwists ct (FaceTwist f d t) = Map.alter applyTwist (f, d) ct
  where applyTwist = toMaybe . ($* t) . fromMaybe
        fromMaybe Nothing = one
        fromMaybe (Just t) = t
        toMaybe t
          | t == one  = Nothing
          | otherwise = Just t
