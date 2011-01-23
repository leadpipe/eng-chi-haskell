{-# LANGUAGE FlexibleContexts #-}

-- | Functions for finding algorithms that satisfy some given conditions.
module Rubik.Searching where

import Rubik.Algebra
import Rubik.Puzzle

import Control.Monad
import Control.Monad.Random
import Control.Parallel.Strategies
import Data.Set (Set)
import qualified Data.Set as Set

-- | Given a list of root nodes, and a way to calculate the children of a node,
-- does a parallel depth-first walk of the implied trees looking for nodes that
-- satisfy a given predicate.  We wrap the search in a monad, to allow for
-- randomness and I/O in the callbacks.
--
-- Typically the trees in question will be successive algorithms through a
-- Rubik-style puzzle.  We've generalized the types to allow for additional
-- state to be included with the algorithms, for efficiency's sake: this extra
-- state can be maintained incrementally rather than recalculated for each node.
searchForest :: (Monad m) => [m a] -> (a -> m [a]) -> (a -> m Bool) -> m [a]
searchForest roots calcChildren satisfies = concatSequence searchedTrees
  where searchedTrees = [(root >>= searchTree) `using` rpar | root <- roots]

        --searchTree :: a -> m [a]
        searchTree node = do
          sat <- satisfies node
          if sat then return [node] else do
            children <- calcChildren node
            concatSequence (map searchTree children)

        --concatSequence :: [m [a]] -> m [a]
        concatSequence lml = liftM concat $ sequence lml


-- | For nodes that encapsulate Rubik-style algorithms, produces a list of
-- successor algorithms given a way to generate moves.
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
