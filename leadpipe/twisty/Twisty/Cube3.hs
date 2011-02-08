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

{-# LANGUAGE TypeFamilies #-}
-- | Defines the basic 3x3 cube puzzle.
module Twisty.Cube3 where

import Twisty.Cycles
import Twisty.Cube
import Twisty.Group
import Twisty.Polyhedron
import Twisty.Puzzle
import Twisty.Wreath

import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Data.Monoid (Monoid, mappend, mempty)

newtype Cube3 = Cube3 (Wreath Vertex, Wreath Edge) deriving (Eq, Ord)

instance Monoid Cube3 where
  mempty = Cube3 one
  mappend (Cube3 s1) (Cube3 s2) = Cube3 (s1 $* s2)

instance Group Cube3 where
  ginvert (Cube3 s) = Cube3 (ginvert s)

instance Puzzle Cube3 where
  type Move Cube3 = FaceTwist
  fromMove (FaceTwist f 1) = Cube3 (v, e)
    where v = fromCycles [asCycle' f faceVertices vertexFaces]
          e = fromCycles [asCycle' f faceEdges edgeFaces]
  fromMove (FaceTwist f n) = fromMove (FaceTwist f 1) $^ n

instance Show Cube3 where
  showsPrec _ (Cube3 (v, e)) = fromOptCycles $ optShowCycles v $* optShowCycles e

c3 :: String -> Algorithm Cube3
c3 = read