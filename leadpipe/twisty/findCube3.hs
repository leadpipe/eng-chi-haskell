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
import Twisty.Cube3
import Twisty.Cycles
import Twisty.Group
import Twisty.FaceTwist
import Twisty.Polyhedron (faceEdges, faceVertices)
import Twisty.Puzzle
import Twisty.Searching
import Twisty.Twists
import Twisty.Wreath (leavesAllUnaltered)
import Twisty.Zn

import Control.Monad.Random
import Control.Parallel.Strategies
import Data.List ((\\))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid (Any(..))
import System.Random

type SearchM a = Rand StdGen a
type Node = (Algorithm Cube3, CubeTwists1)

main = do
  let roots = cycle $ map (makeRoot . read) ["f+", "f="]
  let search = searchTree calcChildren (return . whatWe'reLookingFor . fst)
  gens <- stdGenStream  -- seededStdGens 0
  let nodes = concat (zipWith (evalRand . search) roots gens `using` parBuffer 3 rseq)
  mapM_ (print . fst) nodes

makeRoot :: CubeMove1 -> SearchM Node
makeRoot mv = return (one `applyMove` mv, emptyTwists `updateTwists` mv)

calcChildren :: Node -> Bool -> SearchM [Node]
calcChildren node _ = do algs <- generateChildrenToLength 12 fst genMove 3 node
                         return $ map addTwists algs
  where addTwists alg = (alg, snd node `updateTwists` lastMove alg)


whatWe'reLookingFor :: Algorithm Cube3 -> Bool
whatWe'reLookingFor a = moveCount a > 4
                        && v `leavesAllUnaltered` nonUpVertices
                        && e `leavesAllUnaltered` nonUpEdges
                        && lastMoveFace /= U
                        && s /= one
  where Cube3 s@(v, e) = result a
        FaceTwist lastMoveFace _ _ = lastMove a

nonUpVertices :: [Vertex]
nonUpVertices = [minBound..] \\ faceVertices U

nonUpEdges :: [Edge]
nonUpEdges = [minBound..] \\ faceEdges U

-- | Generates a random move, sometimes using the given algorithm to close out
-- the cumulative twist for a face.
genMove :: Node -> SearchM CubeMove1
genMove node@(alg, twists) = do
  let move = lastMove alg
  if nothingApplicable twists move
    then randomMove
    else do i <- getRandomR (1::Int, 10)
            if i <= 3 then randomMove else do
              let ats = applicableTwists twists move
              j <- getRandomR (0, length ats - 1)
              let ((f, d), t) = ats !! j
              return (FaceTwist f d (-t))
    where randomMove = do
            f <- getRandomR (fromEnum (minBound::Face), fromEnum (maxBound::Face))
            t <- getRandomR (1::Int, 3)
            return (FaceTwist (toEnum f) 0 (toEnum t))
