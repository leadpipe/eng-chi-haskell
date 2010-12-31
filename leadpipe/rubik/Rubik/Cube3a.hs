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

newtype Cube3a = Cube3a (Cube3, DirectionalFaceWreath) deriving (Eq, Ord)

instance Monoid Cube3a where
  mempty = Cube3a one
  mappend (Cube3a s1) (Cube3a s2) = Cube3a (s1 *> s2)

instance Group Cube3a where
  ginvert (Cube3a s) = Cube3a (ginvert s)

instance Puzzle Cube3a Face where
  numLayers _ _ = 1
  numTwists _ _ = 4
  fromFaceTwist f 0 = Cube3a ((fromFaceTwist f 0), (fromCycles [[WM (fromEnum f) 1]]))

instance Show Cube3a where
  showsPrec _ (Cube3a ((Cube3 (v, e)), f)) = fromOptCycles $ showVertices *> showEdges *> showFaces
    where showVertices = optShowCyclesDefault Vertex v
          showEdges = optShowCyclesDefault Edge e
          showFaces = optShowCyclesDefault (toEnum::Int->Face) f
