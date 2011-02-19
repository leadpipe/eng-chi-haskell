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

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

-- | Operators for transforming algorithms of cube-based puzzles according to
-- the symmetries of the cube.
module Twisty.CubeSymmetries where

import Twisty.Cube
import Twisty.FaceTwist
import Twisty.Polyhedron
import Twisty.Puzzle

-- | Transforms a move by rotating it on the cube a quarter turn clockwise
-- around the given face.
rotateMove :: (Ord d) => Face -> CubeMove d -> CubeMove d
rotateMove f m@(FaceTwist f2 t d)
  | f2 `neighbors` f = FaceTwist (nextNeighbor f f2) t d
  | otherwise        = m

-- | Transforms a move by reflecting it on the cube parallel to the given face.
reflectMove :: (Ord d) => Face -> CubeMove d -> CubeMove d
reflectMove f (FaceTwist f2 t d)
  | f2 `neighbors` f = FaceTwist f2 (-t) d
  | otherwise        = FaceTwist (oppositeFace f2) (-t) d

type CubeSymmetry = forall p d. (Puzzle p, Ord d, Move p ~ CubeMove d) => Algorithm p -> Algorithm p

rotU :: CubeSymmetry -- ^ 1/4 turn around UD axis, clockwise according to U.
rotU = morph (rotateMove U)
rotF :: CubeSymmetry -- ^ 1/4 turn around FB axis, clockwise according to F.
rotF = morph (rotateMove F)
rotL :: CubeSymmetry -- ^ 1/4 turn around LR axis, clockwise according to L.
rotL = morph (rotateMove L)
rotR :: CubeSymmetry -- ^ 1/4 turn around LR axis, clockwise according to R.
rotR = morph (rotateMove R)
rotB :: CubeSymmetry -- ^ 1/4 turn around FB axis, clockwise according to B.
rotB = morph (rotateMove B)
rotD :: CubeSymmetry -- ^ 1/4 turn around UD axis, clockwise according to D.
rotD = morph (rotateMove D)

rotUD :: CubeSymmetry -- ^ 1/2 turn around UD axis.
rotUD = rotU . rotU
rotFB :: CubeSymmetry -- ^ 1/2 turn around FB axis.
rotFB = rotF . rotF
rotLR :: CubeSymmetry -- ^ 1/2 turn around LR axis.
rotLR = rotL . rotL

rotFR :: CubeSymmetry -- ^ 1/2 turn around FR edge.
rotFR = rotFB . rotU
rotFL :: CubeSymmetry -- ^ 1/2 turn around FL edge.
rotFL = rotU . rotFB
rotUL :: CubeSymmetry -- ^ 1/2 turn around UL edge.
rotUL = rotUD . rotF
rotUR :: CubeSymmetry -- ^ 1/2 turn around UR edge.
rotUR = rotF . rotUD
rotUB :: CubeSymmetry -- ^ 1/2 turn around UB edge.
rotUB = rotUD . rotL
rotUF :: CubeSymmetry -- ^ 1/2 turn around UF edge.
rotUF = rotL . rotUD

rotURF :: CubeSymmetry -- ^ 1/3 turn around diagonal.
rotURF = rotF . rotU
rotUFR :: CubeSymmetry -- ^ 1/3 turn around diagonal.
rotUFR = rotD . rotB
rotUFL :: CubeSymmetry -- ^ 1/3 turn around diagonal.
rotUFL = rotU . rotF
rotULF :: CubeSymmetry -- ^ 1/3 turn around diagonal.
rotULF = rotB . rotD
rotURB :: CubeSymmetry -- ^ 1/3 turn around diagonal.
rotURB = rotF . rotD
rotUBR :: CubeSymmetry -- ^ 1/3 turn around diagonal.
rotUBR = rotU . rotB
rotUBL :: CubeSymmetry -- ^ 1/3 turn around diagonal.
rotUBL = rotD . rotF
rotULB :: CubeSymmetry -- ^ 1/3 turn around diagonal.
rotULB = rotB . rotU

mirUD :: CubeSymmetry -- ^ Reflection across UD axis.
mirUD = morph (reflectMove U)
mirFB :: CubeSymmetry -- ^ Reflection across FB axis.
mirFB = morph (reflectMove F)
mirLR :: CubeSymmetry -- ^ Reflection across LR axis.
mirLR = morph (reflectMove L)

rotMirU :: CubeSymmetry -- ^ 1/4 turn around, and reflection across, UD axis, clockwise according to U.
rotMirU = rotU . mirUD
rotMirF :: CubeSymmetry -- ^ 1/4 turn around, and reflection across, FB axis, clockwise according to F.
rotMirF = rotF . mirFB
rotMirL :: CubeSymmetry -- ^ 1/4 turn around, and reflection across, LR axis, clockwise according to L.
rotMirL = rotL . mirLR
rotMirR :: CubeSymmetry -- ^ 1/4 turn around, and reflection across, UD axis, clockwise according to R.
rotMirR = rotR . mirLR
rotMirB :: CubeSymmetry -- ^ 1/4 turn around, and reflection across, FB axis, clockwise according to B.
rotMirB = rotB . mirFB
rotMirD :: CubeSymmetry -- ^ 1/4 turn around, and reflection across, LR axis, clockwise according to D.
rotMirD = rotD . mirUD

mirUL :: CubeSymmetry -- ^ Reflection across UL edge.
mirUL = rotF . mirUD
mirUR :: CubeSymmetry -- ^ Reflection across UR edge.
mirUR = mirUD . rotF
mirUF :: CubeSymmetry -- ^ Reflection across UF edge.
mirUF = mirUD . rotL
mirUB :: CubeSymmetry -- ^ Reflection across UB edge.
mirUB = rotL . mirUD
mirFL :: CubeSymmetry -- ^ Reflection across FL edge.
mirFL = mirFB . rotU
mirFR :: CubeSymmetry -- ^ Reflection across FR edge.
mirFR = rotU . mirFB

rotMirUFL :: CubeSymmetry -- ^ 1/6 turn around, and reflection across, diagonal.
rotMirUFL = rotURF . mirFB
rotMirUBL :: CubeSymmetry -- ^ 1/6 turn around, and reflection across, diagonal.
rotMirUBL = mirFB . rotURF
rotMirULB :: CubeSymmetry -- ^ 1/6 turn around, and reflection across, diagonal.
rotMirULB = rotUFR . mirFB
rotMirULF :: CubeSymmetry -- ^ 1/6 turn around, and reflection across, diagonal.
rotMirULF = mirFB . rotUFR
rotMirURF :: CubeSymmetry -- ^ 1/6 turn around, and reflection across, diagonal.
rotMirURF = rotUBL . mirFB
rotMirURB :: CubeSymmetry -- ^ 1/6 turn around, and reflection across, diagonal.
rotMirURB = mirFB . rotUBL
rotMirUFR :: CubeSymmetry -- ^ 1/6 turn around, and reflection across, diagonal.
rotMirUFR = mirFB . rotULB
rotMirUBR :: CubeSymmetry -- ^ 1/6 turn around, and reflection across, diagonal.
rotMirUBR = rotULB . mirFB

mir :: CubeSymmetry -- ^ Reflection across center of cube.
mir = mirUD . mirFB . mirLR
