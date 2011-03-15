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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Functions and types for finding algorithms that satisfy some given
-- conditions.
module Twisty.Searching where

import Twisty.Puzzle

import Control.Monad
import Control.Monad.Random
import Control.Parallel.Strategies
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Conc (numCapabilities)


-- | Given a root node, and a way to calculate the children of a node, does a
-- parallel depth-first walk of the implied tree looking for nodes that satisfy
-- a given predicate.  We wrap the search in a monad, to allow for randomness
-- and I/O in the callbacks.
--
-- Typically the trees in question will be successive algorithms through a
-- twisty puzzle.  We've generalized the types to allow for additional state to
-- be included with the algorithms, for efficiency's sake: this extra state can
-- be maintained incrementally rather than recalculated for each node.
searchTree :: (Monad m) =>
              (a -> Bool -> m [a]) ->
                -- ^ Calculates some child nodes; the 2nd argument is whether
                -- the given node satisfies the predicate (ie will be returned).
              (a -> m Bool) ->
                -- ^ The predicate: tells whether a node should be returned by
                -- this function.
              m a ->
                -- ^ The root node at which to start searching.
              m [a]
searchTree calcChildren satisfies root = root >>= st
  where st node = do
          sat <- satisfies node
          let this = [return [node] | sat]
          children <- calcChildren node sat
          let results = this ++ map st children
          concatM results


concatM :: (Monad m) => [m [a]] -> m [a]
concatM = liftM concat . sequence


-- | A node in a searchable tree that contains an algorithm.
class (Puzzle (NodePuzzle n), Ord (Move (NodePuzzle n))) => AlgorithmNode n where

  type NodePuzzle n
  -- ^ The associated puzzle type.

  getAlgorithm :: n -> Algorithm (NodePuzzle n)
  -- ^ Retrieves the algorithm.

  makeRootNode :: Algorithm (NodePuzzle n) -> n
  -- ^ Makes a node from an algorithm.

  makeChildNode :: n -> Move (NodePuzzle n) -> n
  -- ^ Given the parent node and a move to apply to the node's algorithm,
  -- produces a node encapsulating the resulting algorithm.


-- | An algorithm is a degenerate 'AlgorithmNode'.
instance (Puzzle p, Ord (Move p)) => AlgorithmNode (Algorithm p) where
  type NodePuzzle (Algorithm p) = p
  getAlgorithm = id
  makeRootNode = id
  makeChildNode = applyMove


-- | A node in a searchable tree that contains an algorithm and a way to
-- generate a successor move.
class (AlgorithmNode n, Monad (GenMonad n)) => SearchNode n where

  type GenMonad n :: * -> *
  -- ^ The associated monad for generating moves.

  generateMove :: n -> GenMonad n (Move (NodePuzzle n))
  -- ^ Generates a move to apply to the algorithm.


-- | For 'SearchNode's, produces a list of successor nodes.  Only produces
-- algorithms that are true extensions to the parent algorithm: moves that undo
-- the last move or that are ordered before the last move are discarded.
generateChildren :: SearchNode n =>
                    Int ->      -- ^ How many unique children to produce.
                    n ->        -- ^ The parent node.
                    GenMonad n [n] -- ^ The children nodes.
generateChildren count node = collect 0 Set.empty []
  where alg = getAlgorithm node
        collect n seen nodes
          | n >= count  = return nodes
          | otherwise   = do mv <- generateMove node
                             if mv `Set.member` seen then collect n seen nodes
                               else let node' = node `makeChildNode` mv
                                        alg' = getAlgorithm node'
                                        seen' = mv `Set.insert` seen
                                    in if isNontrivial alg' && lastMove alg' == mv
                                       then collect (n+1) seen' (node':nodes)
                                       else collect n seen' nodes

-- | Stops generating children after reaching algorithms of a particular length.
generateChildrenToLength :: SearchNode n => Int -> Int -> Bool -> n -> Bool -> GenMonad n [n]
generateChildrenToLength len count stop node satisfied =
  if stop && satisfied || moveCount (getAlgorithm node) >= len
  then return []
  else generateChildren count node


-- | Returns a stream of deterministic random number generators given a seed.
seededStdGens :: Int -> IO [StdGen]
seededStdGens = return . stdGenToStream . mkStdGen

-- | A stream of random number generators.
stdGenStream :: IO [StdGen]
stdGenStream = fmap stdGenToStream newStdGen

-- | Returns a stream of random number generators given an initial generator.
stdGenToStream :: StdGen -> [StdGen]
stdGenToStream gen = let (g1, g2) = split gen in g1 : stdGenToStream g2


-- | A monad capable of being evaluated with a standard random number generator.
class Monad m => RandMonad m where

  evalRandMonad :: m a -> StdGen -> a
  -- ^ Runs the monadic value with the given generator.


-- | A useful 'GenMonad' type.
type SearchM = Rand StdGen

instance RandMonad SearchM where
  evalRandMonad = evalRand


-- | Generalized searcher using 'SearchNode'.
searchNodeTree :: (SearchNode n, Read (Move (NodePuzzle n)), RandMonad (GenMonad n)) =>
                  (n -> Bool -> GenMonad n [n]) ->      -- ^ Child-node generator.
                  (n -> GenMonad n Bool) ->             -- ^ Predicate.
                  IO [StdGen] ->                        -- ^ Stream of rand generators.
                  [String] ->                           -- ^ Starting algorithms in string form.
                  (n -> IO ()) ->                       -- ^ Operation to perform on each found node.
                  IO ()
searchNodeTree calcChildren satisfies generatorStream starts op = do
  let roots = map (return . makeRootNode . read) starts
  let randSearch = evalRandMonad . searchTree calcChildren satisfies
  gens <- generatorStream
  let nodesList = zipWith randSearch roots gens `using` parBuffer numCapabilities rseq
  mapM_ op $ concat nodesList


-- | Searches forever using nondeterministic generators.
searchForever :: (SearchNode n, Read (Move (NodePuzzle n)), RandMonad (GenMonad n)) =>
                 (n -> Bool -> GenMonad n [n]) ->       -- ^ Child-node generator.
                 (Algorithm (NodePuzzle n) -> Bool) ->  -- ^ Predicate.
                 [String] ->                            -- ^ Starting algorithms in string form.
                 (n -> IO ()) ->                        -- ^ Operation to perform on each found node.
                 IO ()
searchForever calcChildren satisfies =
  searchNodeTree calcChildren (return . satisfies . getAlgorithm) stdGenStream . cycle


-- | Searches once using seeded generators.
searchOnce    :: (SearchNode n, Read (Move (NodePuzzle n)), RandMonad (GenMonad n)) =>
                 Int ->                                 -- ^ The seed for the generators.
                 (n -> Bool -> GenMonad n [n]) ->       -- ^ Child-node generator.
                 (Algorithm (NodePuzzle n) -> Bool) ->  -- ^ Predicate.
                 [String] ->                            -- ^ Starting algorithms in string form.
                 (n -> IO ()) ->                        -- ^ Operation to perform on each found node.
                 IO ()
searchOnce seed calcChildren satisfies =
  searchNodeTree calcChildren (return . satisfies . getAlgorithm) (seededStdGens seed)
