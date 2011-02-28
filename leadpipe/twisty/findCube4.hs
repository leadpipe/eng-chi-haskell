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
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Twisty.Cube
import Twisty.Cube4
import Twisty.Cycles
import Twisty.Group
import Twisty.FaceTwist
import Twisty.Puzzle
import Twisty.Searching
import Twisty.Twists
import Twisty.Wreath (numIndicesMoved)
import Twisty.Zn

import Control.Monad.Random
import Control.Parallel.Strategies
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid (Any(..))
import System.Random

type SearchM a = Rand StdGen a
type Node = (Algorithm Cube4, CubeTwists2)

main = do
  let roots = cycle $ map (makeRoot . read) ["f+", "F+", "f=", "F="]
  let search = searchTree calcChildren (return . whatWe'reLookingFor . fst)
  gens <- stdGenStream  -- seededStdGens 0
  let nodes = concat (zipWith (evalRand . search) roots gens `using` parBuffer 3 rseq)
  mapM_ (print . fst) nodes

makeRoot :: CubeMove2 -> SearchM Node
makeRoot mv = return (one `applyMove` mv, emptyTwists `updateTwists` mv)

calcChildren :: Node -> Bool -> SearchM [Node]
calcChildren node _ = do algs <- generateChildrenToLength 20 fst genMove 2 node
                         return $ map addTwists algs
  where addTwists alg = (alg, snd node `updateTwists` lastMove alg)


whatWe'reLookingFor :: Algorithm Cube4 -> Bool
whatWe'reLookingFor a = numEdges > 0 && numEdges <= 4 && moveCount a > 4 && getAny hasInnerTwist && facePiecesStay
  where Cube4 (_, e, f) = result a
        numEdges = numIndicesMoved e
        hasInnerTwist = foldMoves (\(FaceTwist _ d _) -> Any (d == 1)) a
        facePiecesStay = True -- TODO: idea is, ensure face pieces don't map to different faces


-- | Generates a random move, sometimes using the given algorithm to close out
-- the cumulative twist for a face.
genMove :: Node -> SearchM CubeMove2
genMove node@(alg, twists) = do
  let (FaceTwist lf ld _) = lastMove alg
  let lastIndex = (lf, ld)
  if nothingApplicable twists lastIndex
    then randomMove
    else do i <- getRandomR (1::Int, 10)
            if i <= 3 then randomMove else do
              let ats = applicableTwists twists lastIndex
              j <- getRandomR (0, length ats - 1)
              let ((f, d), t) = ats !! j
              return (FaceTwist f d (-t))
    where randomMove = do
            f <- getRandomR (fromEnum (minBound::Face), fromEnum (maxBound::Face))
            d <- getRandomR (1::Int, 5) -- 40% chance of both layers, 60% outer layer only
            t <- getRandomR (1::Int, 3)
            return (FaceTwist (toEnum f) (if d > 2 then 0 else 1) (toEnum t))
