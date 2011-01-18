{-# LANGUAGE FlexibleContexts #-}

-- | Functions for finding algorithms that satisfy some given conditions.
module Rubik.Searching where

import Rubik.Algebra
import Rubik.Puzzle

import Control.Monad (liftM2)
import Control.Monad.Random
--import Control.Monad.State
import Data.Set (Set)
import qualified Data.Set as Set

-- | Given an initial list of moves, searches a random tree of algorithms
-- looking for instances that satisfy a given predicate.  The resulting list
-- lives in the RandT + State monad.
findAlgorithms :: (Puzzle s, RandomGen g, Random (Move s), Ord (Move s)) =>
                  [Move s] -> (Algorithm s -> Bool) -> (Algorithm s -> Int -> Bool)
                  -> Rand g [Algorithm s]
findAlgorithms [] _ _ = return []
findAlgorithms (m:ms) satisfying continue = liftM2 (++) fa (findAlgorithms ms satisfying continue)
  where fa = fa' (applyMove one m)
        fa' alg = if satisfying alg then return [alg] else walkFrom alg 0 Set.empty
        --walkFrom :: Algorithm s -> Int -> Set (Move s) -> Rand g [Algorithm s]
        walkFrom alg count tried = if continue alg count then try alg count tried else return []
        try alg count tried =
          do m <- getRandom
             if m `Set.member` tried then try alg count tried
               else let alg' = alg `applyMove` m
                        tried' = m `Set.insert` tried
                        ans = if isNontrivial alg' && lastMove alg' == m
                              then fa' alg' else return []
                    in liftM2 (++) ans (walkFrom alg (count+1) tried')
