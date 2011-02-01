{-# LANGUAGE TypeFamilies #-}
-- | Defines the 3x3 tetrahedron puzzle.
module Rubik.Tetra3 where

import Rubik.Cycles
import Rubik.Group
import Rubik.Polyhedron
import Rubik.Puzzle
import Rubik.Tetra
import Rubik.Wreath

import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Data.Monoid (Monoid, mappend, mempty)

newtype Tetra3 = Tetra3 (Wreath Vertex, Wreath Edge) deriving (Eq, Ord)

instance Monoid Tetra3 where
  mempty = Tetra3 one
  mappend (Tetra3 s1) (Tetra3 s2) = Tetra3 (s1 $* s2)

instance Group Tetra3 where
  ginvert (Tetra3 s) = Tetra3 (ginvert s)

instance Puzzle Tetra3 where
  type Move Tetra3 = FaceTwist
  fromMove (FaceTwist f 1) = Tetra3 (v, e)
    where v = fromCycles [asCycle' f faceVertices vertexFaces]
          e = fromCycles [asCycle' f faceEdges edgeFaces]
  fromMove (FaceTwist f n) = fromMove (FaceTwist f 1) $^ n

instance Show Tetra3 where
  showsPrec _ (Tetra3 (v, e)) = fromOptCycles $ optShowCycles v $* optShowCycles e

t3 :: String -> Algorithm Tetra3
t3 = read
