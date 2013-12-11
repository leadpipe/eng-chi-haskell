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

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

-- | Operators for transforming algorithms of tetra-based puzzles according to
-- the symmetries of the tetra.
module Twisty.TetraSymmetries where

import Twisty.Tetra
import Twisty.FaceTwist
import Twisty.Polyhedron
import Twisty.Puzzle

-- | Transforms a move by rotating it on the tetra a 1/3 turn clockwise around
-- the given face.
rotateMove :: (Ord d) => Face -> TetraMove d -> TetraMove d
rotateMove f m@(FaceTwist f2 d t)
  | f2 == f   = m
  | otherwise = FaceTwist (nextNeighbor f f2) d t

-- | Transforms a move by reflecting it on the tetra parallel to the edge
-- between the two given faces, such that the two faces are swapped.
reflectMove :: (Ord d) => Face -> Face -> TetraMove d -> TetraMove d
reflectMove f1 f2 (FaceTwist f3 d t)
  | f3 == f1  = FaceTwist f2 d (-t)
  | f3 == f2  = FaceTwist f1 d (-t)
  | otherwise = FaceTwist f3 d (-t)

type TetraSymmetry = forall p d. (Puzzle p, Ord d, Move p ~ TetraMove d) => Algorithm p -> Algorithm p

rotF :: TetraSymmetry -- ^ 1/3 turn around F, clockwise.
rotF = morph (rotateMove F)
rotL :: TetraSymmetry -- ^ 1/3 turn around L, clockwise.
rotL = morph (rotateMove L)
rotR :: TetraSymmetry -- ^ 1/3 turn around R, clockwise.
rotR = morph (rotateMove R)
rotD :: TetraSymmetry -- ^ 1/3 turn around D, clockwise.
rotD = morph (rotateMove D)

rotF2 :: TetraSymmetry -- ^ 1/3 turn around F, counterclockwise.
rotF2 = rotF . rotF
rotL2 :: TetraSymmetry -- ^ 1/3 turn around L, counterclockwise.
rotL2 = rotL . rotL
rotR2 :: TetraSymmetry -- ^ 1/3 turn around R, counterclockwise.
rotR2 = rotR . rotR
rotD2 :: TetraSymmetry -- ^ 1/3 turn around D, counterclockwise.
rotD2 = rotD . rotD

mirFL :: TetraSymmetry -- ^ Reflection swapping F and L.
mirFL = morph (reflectMove F L)
mirFR :: TetraSymmetry -- ^ Reflection swapping F and R.
mirFR = morph (reflectMove F R)
mirFD :: TetraSymmetry -- ^ Reflection swapping F and D.
mirFD = morph (reflectMove F D)
mirLR :: TetraSymmetry -- ^ Reflection swapping L and R.
mirLR = morph (reflectMove L R)
mirLD :: TetraSymmetry -- ^ Reflection swapping L and D.
mirLD = morph (reflectMove L D)
mirRD :: TetraSymmetry -- ^ Reflection swapping R and D.
mirRD = morph (reflectMove R D)

rotFL :: TetraSymmetry -- ^ 1/2 turn around FL edge (and RD).
rotFL = mirFL . mirRD
rotFR :: TetraSymmetry -- ^ 1/2 turn around FR edge (and LD).
rotFR = mirFR . mirLD
rotFD :: TetraSymmetry -- ^ 1/2 turn around FD edge (and LR).
rotFD = mirFD . mirLR
