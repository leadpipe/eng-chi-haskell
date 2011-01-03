{-# LANGUAGE TypeFamilies #-}
-- | Defines the "ad-supported" 3x3 cube puzzle with faces that have a
-- right way up.
module Rubik.Cube3a where

import Rubik.Algebra
import Rubik.Cycles
import Rubik.Cube
import Rubik.Cube3
import Rubik.Puzzle

import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Data.Monoid (Monoid, mappend, mempty)

type DirectionalFaceWreath = Wreath Twist4

newtype Cube3a = Cube3a (Cube3, DirectionalFaceWreath) deriving (Eq, Ord)

instance Monoid Cube3a where
  mempty = Cube3a one
  mappend (Cube3a s1) (Cube3a s2) = Cube3a (s1 *> s2)

instance Group Cube3a where
  ginvert (Cube3a s) = Cube3a (ginvert s)

instance Puzzle Cube3a where
  type Move Cube3a = FaceTwist
  fromMove m@(FaceTwist f 1) = Cube3a (fromMove m, fromCycles [[WM (fromEnum f, 1)]])
  fromMove (FaceTwist f n) = fromMove (FaceTwist f 1) ^> n

instance Show Cube3a where
  showsPrec _ (Cube3a ((Cube3 (v, e)), f)) = fromOptCycles $ showVertices *> showEdges *> showFaces
    where showVertices = optShowCyclesDefault Vertex v
          showEdges = optShowCyclesDefault Edge e
          showFaces = optShowCyclesDefault (toEnum::Int->Face) f

c3a :: String -> Algorithm Cube3a
c3a = read
