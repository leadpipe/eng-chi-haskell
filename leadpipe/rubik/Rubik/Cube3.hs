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
    where v = fromCycles [asCycle faceVertices vertexFaces]
          e = fromCycles [asCycle faceEdges edgeFaces]
          asCycle :: forall a t. (Enum a, Monoid t, Ord t, Num t) =>
                     (Face -> [a]) -> (a -> [Face]) -> [WreathMove t]
          asCycle toAs toFs = map toWM $ zip as $ rotate 1 as
            where as = toAs f
                  toWM (a1, a2) = WM (fromEnum a1) (twist (toFs a1) (toFs a2))
                  twist fs1 fs2 = fromInteger (indexIn fs1 - indexIn fs2)
                  indexIn fs = toInteger $ fromJust $ f `elemIndex` fs

instance Show Cube3 where
  showsPrec n c@(Cube3 v e) = if c == one then showEmpty else showVertices . showEdges
    where showEmpty = showString "()"
          showVertices = showCycles showVertexMove id v
          showEdges = showCycles showEdgeMove id e
          showVertexMove = showMove Vertex
          showEdgeMove = showMove Edge
