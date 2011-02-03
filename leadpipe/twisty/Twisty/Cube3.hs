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
