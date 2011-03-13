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
import Twisty.Cube3
import Twisty.Group
import Twisty.Polyhedron (faceEdges, faceVertices)
import Twisty.Puzzle
import Twisty.Searching
import Twisty.TwistSearchNode
import Twisty.Wreath (leavesAllUnaltered)
import Twisty.Zn

import Data.List ((\\))
import Data.Ratio ((%))

-- | Our search node type.  This is the same type as 'TwistSearchNode'
-- specialized for the 3x3 cube.
type Node = (Algorithm Cube3, CubeTwists1)
instance SearchNode Node where
  type GenMonad Node = SearchM
  generateMove = generateSimpleTwistSearchNodeMove 0 (3%10)

main = searchForever calcChildren upFaceOnly 3 starts (print . fst)

-- | Use this for profiling
main' = searchOnce calcChildren upFaceOnly 0 3 starts (print . fst)

starts = ["f+", "f="]

calcChildren :: Node -> Bool -> SearchM [Node]
calcChildren = generateChildrenToLength 12 3 True

upFaceOnly :: Algorithm Cube3 -> Bool
upFaceOnly a = moveCount a > 4
               && v `leavesAllUnaltered` nonUpVertices
               && e `leavesAllUnaltered` nonUpEdges
  where Cube3 s@(v, e) = result a

nonUpVertices :: [Vertex]
nonUpVertices = [minBound..] \\ faceVertices U

nonUpEdges :: [Edge]
nonUpEdges = [minBound..] \\ faceEdges U
