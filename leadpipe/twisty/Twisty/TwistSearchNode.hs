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

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | 'TwistSearchNode' pairs an 'Algorithm' with its 'CumulativeTwists' to make
-- it easy to generate good moves while searching the tree of possible
-- algorithms for a twisty puzzle.  This module provides an instance for
-- 'AlgorithmNode', and functions to serve as definitions for 'generateMove' to
-- aid the creation of an instance for 'SearchNode'.
module Twisty.TwistSearchNode where

import Twisty.FaceTwist
import Twisty.Group
import Twisty.Polyhedron
import Twisty.Puzzle
import Twisty.Searching
import Twisty.Zn

import Control.Monad.Random
import Data.Ratio


-- | A node type for 'searchTree' that lets us track cumulative twists.
type TwistSearchNode p f d t = (Algorithm p, CumulativeTwists f d t)

instance (Puzzle p, PolyFace f, Ord d, Group t, Ord t, Move p ~ FaceTwist f d t) =>
         AlgorithmNode (TwistSearchNode p f d t) where
  type NodePuzzle (TwistSearchNode p f d t) = p
  getAlgorithm = fst
  makeRootNode alg = (alg, foldl updateTwists emptyTwists (moves alg))
  makeChildNode node mv = (fst node `applyMove` mv, snd node `updateTwists` mv)


-- | Generates a random move for a 'TwistSearchNode'.  With some probability,
-- chooses a move based on the puzzle's cumulative twist rather than a
-- completely random move.
generateFaceTwist ::
  (Puzzle p, PolyFace f, Bounded f, Enum f, Ord d, Bounded t, Ord t, Enum t, Group t,
   Move p ~ FaceTwist f d t) =>
  (d -> SearchM d)              -- ^ A generator function for the depth of a random move.
  -> Ratio Int                  -- ^ The probability of choosing a completely random move.
  -> TwistSearchNode p f d t    -- ^ The parent node to generate a subsequent move from.
  -> SearchM (FaceTwist f d t)
generateFaceTwist genDepth justRandom node@(alg, twists) =
  if nothingApplicable twists move
    then randomMove
    else do i <- getRandomR (1, denominator justRandom)
            if i <= numerator justRandom
              then randomMove
              else do let ats = applicableTwists twists move
                      j <- getRandomR (0, length ats - 1)
                      let ((f, d), t) = ats !! j
                      return (FaceTwist f d (ginvert t))
    where move = lastMove alg
          FaceTwist face depth twist = move
          minf = minBound `asTypeOf` face
          maxf = maxBound `asTypeOf` face
          mint = minBound `asTypeOf` twist
          maxt = maxBound `asTypeOf` twist
          randomMove = do
            f <- getRandomR (fromEnum minf, fromEnum maxf)
            d <- genDepth depth
            t <- getRandomR (1 + fromEnum mint, fromEnum maxt)
            return (FaceTwist (toEnum f) d (toEnum t))


-- | A depth generator for 'generateFaceTwist' that always returns the same
-- depth.
constantDepth :: d -> d -> SearchM d
constantDepth d = return . const d

-- | Generates an outer-layer face twist.
generateOuterFaceTwist ::
  (Puzzle p, PolyFace f, Bounded f, Enum f, Ord d, Num d, Bounded t, Ord t, Enum t, Group t,
   Move p ~ FaceTwist f d t) =>
  Ratio Int                     -- ^ The probability of choosing a completely random move.
  -> TwistSearchNode p f d t    -- ^ The parent node to generate a subsequent move from.
  -> SearchM (FaceTwist f d t)
generateOuterFaceTwist = generateFaceTwist $ constantDepth 0
