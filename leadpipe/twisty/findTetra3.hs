{-
Copyright 2013 Google Inc.

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

import Data.List
import Twisty.Group
import Twisty.Polyhedron (faceEdges, faceVertices)
import Twisty.Puzzle
import Twisty.Searching
import Twisty.Tetra
import Twisty.Tetra3
import Twisty.TwistSearchNode
import Twisty.Wreath (leavesAllUnaltered, leavesAllUnmoved)
import Twisty.Zn

import Data.List ((\\))
import Data.Ratio ((%))

-- | Our search node type.  This is the same type as 'TwistSearchNode'
-- specialized for the 3x3 tetra.
type Node = (Algorithm Tetra3, TetraTwists1)
instance SearchNode Node where
  type GenMonad Node = SearchM
  generateMove = generateOuterFaceTwist (3%10)

main = searchForever calcChildren downFaceOnly starts (print . fst)

-- | Use this for profiling
main' = searchOnce 0 calcChildren downFaceOnly starts (print . fst)

starts = ["f+"]

calcChildren :: Node -> Bool -> SearchM [Node]
calcChildren = generateChildrenToLength 12 3 True

somethingUnmoved :: Algorithm Tetra3 -> Bool
somethingUnmoved a = moveCount a > 3
                     && (v `leavesAllUnmoved` [minBound..]
                         || e `leavesAllUnmoved` [minBound..])
  where Tetra3 s@(v, e) = result a

downFaceOnly :: Algorithm Tetra3 -> Bool
downFaceOnly a = moveCount a > 3
                 && v `leavesAllUnaltered` nonDownVertices
                 && e `leavesAllUnaltered` nonDownEdges
  where Tetra3 s@(v, e) = result a

nonDownFaceOnly :: Algorithm Tetra3 -> Bool
nonDownFaceOnly a = moveCount a > 3
                    && v `leavesAllUnaltered` downVertices
                    && e `leavesAllUnaltered` downEdges
  where Tetra3 s@(v, e) = result a

downVerticesOrEdges :: Algorithm Tetra3 -> Bool
downVerticesOrEdges a = moveCount a > 3
                        && v `leavesAllUnaltered` nonDownVertices
                        && e `leavesAllUnmoved` [minBound..]
  where Tetra3 s@(v, e) = result a

edgeFlipping :: Algorithm Tetra3 -> Bool
edgeFlipping a = moveCount a > 3
                 && e `leavesAllUnmoved` [minBound..]
                 && not (e `leavesAllUnaltered` [minBound..])
  where Tetra3 s@(v, e) = result a

nonDownVertices :: [Vertex]
nonDownVertices = [minBound..] \\ faceVertices D

nonDownEdges :: [Edge]
nonDownEdges = [minBound..] \\ faceEdges D

downVertices :: [Vertex]
downVertices = sort $ faceVertices D

downEdges :: [Edge]
downEdges = sort $ faceEdges D
