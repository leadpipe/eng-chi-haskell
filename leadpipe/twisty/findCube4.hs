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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import Twisty.Cube
import Twisty.Cube4
import Twisty.Cycles
import Twisty.Group
import Twisty.FaceTwist
import Twisty.Puzzle
import Twisty.Searching
import Twisty.TwistSearchNode
import Twisty.Twists
import Twisty.Wreath (numIndicesMoved)
import Twisty.Zn

import Control.Monad.Random
import Data.Monoid (Any(..))
import Data.Ratio ((%))

type Node = (Algorithm Cube4, CubeTwists2)
instance SearchNode Node where
  type GenMonad Node = SearchM
  generateMove = generateFaceTwist genDepth (3%10)
    where genDepth _ = do
            d <- getRandomR (1::Int, 5)
            return (if d > 2 then 0 else 1) -- 40% chance of both layers, 60% outer only

main = searchForever calcChildren whatWe'reLookingFor starts (print . fst)

starts = ["f+", "F+", "f=", "F="]

calcChildren :: Node -> Bool -> SearchM [Node]
calcChildren = generateChildrenToLength 12 3 True


whatWe'reLookingFor :: Algorithm Cube4 -> Bool
whatWe'reLookingFor a = numEdges > 0 && numEdges <= 4 && moveCount a > 4 && getAny hasInnerTwist && facePiecesStay
  where Cube4 (_, e, f) = result a
        numEdges = numIndicesMoved e
        hasInnerTwist = foldMoves (\(FaceTwist _ d _) -> Any (d == 1)) a
        facePiecesStay = True -- TODO: idea is, ensure face pieces don't map to different faces
