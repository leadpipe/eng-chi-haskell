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
data (PolyFace face, Group twist, Ord twist, Ord depth) =>
     FaceTwist face twist depth = FaceTwist face twist depth deriving (Eq, Bounded, Ix)

instance (PolyFace f, Group t, Ord t, Ord d) => Ord (FaceTwist f t d) where
  -- | Larger depths compare as less, to match the normal way of twisting:
  -- deeper moves come before shallower ones.
  compare (FaceTwist f1 t1 d1) (FaceTwist f2 t2 d2) = compare (f1, d2, t1) (f2, d1, t2)

instance (PolyFace f, Group t, Bounded t, Ord t, Ix t, Bounded d, Ord d, Ix d) =>
         PuzzleMove (FaceTwist f t d) where
  undoMove (FaceTwist f t d) = FaceTwist f (ginvert t) d

  joinMoves = table (Memo.array jm)
    where table memo move1 move2 = memo (move1, move2)
          jm (m1@(FaceTwist f1 t1 d1), m2@(FaceTwist f2 t2 d2))
            | f1 == f2 && d1 == d2   = let t = t1 $* t2 in
                                       if t == one then [] else [FaceTwist f1 t d1]
            | f1 `neighbors` f2      = [m1, m2]
            | otherwise              = [max m1 m2, min m1 m2]

  isTrivialMove (FaceTwist _ t _) = t == one

-- These default defs cause overlapping instances:
-- instance (PolyFace f, Show f, Group t, Ord t, Show t, Ord d, Show d) =>
--          Show (FaceTwist f t d) where
--   showsPrec _ (FaceTwist f t d) = shows f . shows d . shows t

-- instance (PolyFace f, Read f, Group t, Ord t, Read t, Ord d, Read d) =>
--          Read (FaceTwist f t d) where
--   readsPrec _ "" = []
--   readsPrec _ (c:s) = maybeToList $ do
--     f <- nameToMaybeFace c
--     (d, s') <- listToMaybe (reads s)
--     (t, s'') <- listToMaybe (reads s')
--     return (FaceTwist f t d, s'')


-- | Adds up all the twists associated with each (face, depth) pair.
type CumulativeTwists f t d = Map (f, d) t

emptyTwists :: CumulativeTwists f t d
emptyTwists = Map.empty

updateTwists :: (PolyFace f, Group t, Ord t, Ord d) =>
                CumulativeTwists f t d -> FaceTwist f t d -> CumulativeTwists f t d
updateTwists ct (FaceTwist f t d) = Map.alter applyTwist (f, d) ct
  where applyTwist = toMaybe . ($* t) . fromMaybe
        fromMaybe Nothing = one
        fromMaybe (Just t) = t
        toMaybe t
          | t == one  = Nothing
          | otherwise = Just t
