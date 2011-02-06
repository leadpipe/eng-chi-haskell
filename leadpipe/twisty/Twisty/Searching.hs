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

{-# LANGUAGE FlexibleContexts #-}

-- | Functions for finding algorithms that satisfy some given conditions.
module Twisty.Searching where

import Twisty.Group
import Twisty.Puzzle

import Control.Monad
import Control.Monad.Random
import Control.Parallel.Strategies
import Data.Set (Set)
import qualified Data.Set as Set


-- | Given a root node, and a way to calculate the children of a node, does a
-- parallel depth-first walk of the implied tree looking for nodes that satisfy
-- a given predicate.  We wrap the search in a monad, to allow for randomness
-- and I/O in the callbacks.
--
-- Typically the trees in question will be successive algorithms through a
-- twisty puzzle.  We've generalized the types to allow for additional state to
-- be included with the algorithms, for efficiency's sake: this extra state can
-- be maintained incrementally rather than recalculated for each node.
searchTree :: (Monad m) => (a -> Bool -> m [a]) -> (a -> m Bool) -> m a -> m [a]
searchTree calcChildren satisfies root = root >>= st
  where st node = do
          sat <- satisfies node
          let this = if sat then [return [node]] else []
          children <- calcChildren node sat
          let results = this ++ map st children
          concatM results


concatM :: (Monad m) => [m [a]] -> m [a]
concatM = liftM concat . sequence

-- | For nodes that encapsulate twisty algorithms, produces a list of successor
-- algorithms given a way to generate moves.
generateChildren :: (Monad m, Puzzle p, Ord (Move p)) =>
                    (a -> Algorithm p) -> (a -> m (Move p)) -> Int -> a -> m [Algorithm p]
generateChildren getAlg genMove count node = collect 0 Set.empty []
  where alg = getAlg node
        collect n seen algs
          | n >= count  = return algs
          | otherwise   = do mv <- genMove node
                             if mv `Set.member` seen then collect n seen algs
                               else let alg' = alg `applyMove` mv
                                        seen' = mv `Set.insert` seen
                                    in if isNontrivial alg' && lastMove alg' == mv
                                       then collect (n+1) seen' (alg':algs)
                                       else collect n seen' algs

-- | Stops generating children after reaching algorithms of a particular length.
generateChildrenToLength :: (Monad m, Puzzle p, Ord (Move p)) =>
                            Int -> (a -> Algorithm p) -> (a -> m (Move p)) -> Int -> a -> m [Algorithm p]
generateChildrenToLength len getAlg genMove count node =
  if (length . moves . getAlg) node >= len
  then return []
  else generateChildren getAlg genMove count node
