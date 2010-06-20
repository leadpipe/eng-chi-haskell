{-# LANGUAGE MultiParamTypeClasses, ScopedTypeVariables, TypeSynonymInstances #-}
-- | Defines the "ad-supported" 3x3 cube puzzle with faces that have a
-- right way up.
module Rubik.Cube3a where

import Rubik.Algebra
import Rubik.Cycles
import Rubik.Cube
import Rubik.Cube3
import Rubik.Geometry
import Rubik.Puzzle

import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Data.Monoid (Monoid, mappend, mempty)

data Cube3a = Cube3a Cube3 SquareFaceWreath deriving (Eq, Ord)

instance Monoid Cube3a where
  mempty = Cube3a one one
  mappend (Cube3a c1 f1) (Cube3a c2 f2) = Cube3a (c1 *> c2) (f1 *> f2)

instance Puzzle Cube3a Face where
  fromFaceTwist f 0 = Cube3a (fromFaceTwist f 0) (fromCycles [[WM (fromEnum f) 1]])

instance Show Cube3a where
  showsPrec n ca@(Cube3a c@(Cube3 v e) f) =
    if ca == one then showEmpty else showVertices . showEdges . showFaces
      where showEmpty = showString "()"
            showVertices = showCycles showVertexMove id v
            showEdges = showCycles showEdgeMove id e
            showFaces = showCycles showFaceMove id f
            showVertexMove = showMove Vertex
            showEdgeMove = showMove Edge
            showFaceMove = showMove (toEnum :: Int -> Face)
