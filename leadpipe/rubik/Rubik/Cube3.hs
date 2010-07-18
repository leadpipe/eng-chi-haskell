{-# LANGUAGE MultiParamTypeClasses, ScopedTypeVariables, TypeSynonymInstances #-}
-- | Defines the basic 3x3 cube puzzle.
module Rubik.Cube3 where

import Rubik.Algebra
import Rubik.Cycles
import Rubik.Cube
import Rubik.Geometry
import Rubik.Puzzle

import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Data.Monoid (Monoid, mappend, mempty)

data Cube3 = Cube3 VertexWreath EdgeWreath deriving (Eq, Ord)

instance Monoid Cube3 where
  mempty = Cube3 one one
  mappend (Cube3 v1 e1) (Cube3 v2 e2) = Cube3 (v1 *> v2) (e1 *> e2)

instance Puzzle Cube3 Face where
  fromFaceTwist f 0 = Cube3 v e
    where v = fromCycles [asCycle' f faceVertices vertexFaces]
          e = fromCycles [asCycle' f faceEdges edgeFaces]

instance Show Cube3 where
  showsPrec n c@(Cube3 v e) =
    if c == one then showEmptyParens else showVertices . showEdges
      where showVertices = showNonemptyCycles Vertex v
            showEdges = showNonemptyCycles Edge e
