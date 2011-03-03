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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Functions for finding algorithms that satisfy some given conditions.
module Twisty.Searching where

import Twisty.FaceTwist
import Twisty.Group
import Twisty.Polyhedron
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
          let this = [return [node] | sat]
          children <- calcChildren node sat
          let results = this ++ map st children
          concatM results


concatM :: (Monad m) => [m [a]] -> m [a]
concatM = liftM concat . sequence

-- | For nodes that encapsulate twisty algorithms, produces a list of successor
-- algorithms given a way to generate moves.
generateChildren :: (Monad m, Puzzle p, Ord (Move p)) =>
                    Int -> (a -> Algorithm p) -> (a -> m (Move p)) -> a -> m [Algorithm p]
generateChildren count getAlg genMove node = collect 0 Set.empty []
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
                            Int -> Int -> (a -> Algorithm p) -> (a -> m (Move p)) -> a -> m [Algorithm p]
generateChildrenToLength len count getAlg genMove node =
  if moveCount (getAlg node) >= len
  then return []
  else generateChildren count getAlg genMove node


-- | Returns a stream of deterministic random number generators given a seed.
seededStdGens :: Int -> IO [StdGen]
seededStdGens = return . stdGenToStream . mkStdGen

-- | A stream of random number generators.
stdGenStream :: IO [StdGen]
stdGenStream = fmap stdGenToStream newStdGen

-- | Returns a stream of random number generators given an initial generator.
stdGenToStream :: StdGen -> [StdGen]
stdGenToStream gen = let (g1, g2) = split gen in g1 : stdGenToStream g2


-- | This is just a way to bundle up a bunch of constraints.
class (Puzzle p, PolyFace f, Ord d, Group t, Ord t {-, Move p ~ FaceTwist f d t-}) => PolyPuzzle p f d t
instance (Puzzle p, PolyFace f, Ord d, Group t, Ord t {-, Move p ~ FaceTwist f d t-}) => PolyPuzzle p f d t

-- | A node type for 'searchTree' that lets us track cumulative twists.
type SearchNode p f d t = (Algorithm p, CumulativeTwists f d t)

-- | A useful monad for 'searchTree'.
type SearchM = Rand StdGen

-- | Turns a move into a node.
makeRoot :: --(Puzzle p, PolyFace f, Ord d, Group t, Ord t, Move p ~ FaceTwist f d t, Monad m) =>
            (PolyPuzzle p f d t, Move p ~ FaceTwist f d t, Monad m) =>
            FaceTwist f d t -> m (SearchNode p f d t)
makeRoot mv = return (one `applyMove` mv, emptyTwists `updateTwists` mv)

genNodeChildrenToLength :: (PolyPuzzle p f d t, Monad m, Move p ~ FaceTwist f d t) =>
                           Int -> Int -> (SearchNode p f d t -> m (Move p)) ->
                           SearchNode p f d t -> Bool -> m [SearchNode p f d t]
genNodeChildrenToLength len count genMove node _ =
  do algs <- generateChildrenToLength len count fst genMove node
     return $ map addTwists algs
  where addTwists alg = (alg, snd node `updateTwists` lastMove alg)


-- | Generalized searcher using SearchNode.
searchNodeTree :: (PolyPuzzle p f d t, Move p ~ FaceTwist f d t, Read (Move p), Show (Move p)) =>
                  (SearchNode p f d t -> Bool -> SearchM [SearchNode p f d t]) ->
                  (Algorithm p -> Bool) ->
                  IO [StdGen] ->
                  Int ->
                  [String] ->
                  IO ()
searchNodeTree calcChildren satisfies generatorStream n starts = do
  let roots = map (makeRoot . read) starts
  let search = searchTree calcChildren (return . satisfies . fst)
  gens <- generatorStream
  let nodes = concat (zipWith (evalRand . search) roots gens `using` parBuffer n rseq)
  mapM_ (print . fst) nodes


-- | Searches forever using nondeterministic generators.
searchForever :: (PolyPuzzle p f d t, Move p ~ FaceTwist f d t, Read (Move p), Show (Move p)) =>
                 (SearchNode p f d t -> Bool -> SearchM [SearchNode p f d t]) ->
                 (Algorithm p -> Bool) ->
                 Int ->
                 [String] ->
                 IO ()
searchForever calcChildren satisfies n starts =
  searchNodeTree calcChildren satisfies stdGenStream n (cycle starts)


-- | Searches once using seeded generators.
searchOnce :: (PolyPuzzle p f d t, Move p ~ FaceTwist f d t, Read (Move p), Show (Move p)) =>
              (SearchNode p f d t -> Bool -> SearchM [SearchNode p f d t]) ->
              (Algorithm p -> Bool) ->
              Int ->
              Int ->
              [String] ->
              IO ()
searchOnce calcChildren satisfies n seed =
  searchNodeTree calcChildren satisfies (seededStdGens seed) n
