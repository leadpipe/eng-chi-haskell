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

rotU, rotF, rotL, rotR, rotB, rotD,
  rotUD, rotFB, rotLR,
  rotFR, rotFL, rotUL, rotUR, rotUB, rotUF,
  rotURF, rotUFR, rotUFL, rotULF, rotURB, rotUBR, rotUBL, rotULB,
  mirUD, mirFB, mirLR,
  rotMirU, rotMirF, rotMirL, rotMirR, rotMirB, rotMirD,
  mirUL, mirUR, mirUF, mirUB, mirFL, mirFR,
  rotMirUFL, rotMirUBL, rotMirULB, rotMirULF, rotMirURF, rotMirURB, rotMirUFR, rotMirUBR,
  mir :: (Puzzle p, Ord d, Move p ~ CubeMove d) => Algorithm p -> Algorithm p

rotU = morph (rotateMove U) -- ^ 1/4 turn around UD axis, clockwise according to U.
rotF = morph (rotateMove F) -- ^ 1/4 turn around FB axis, clockwise according to F.
rotL = morph (rotateMove L) -- ^ 1/4 turn around LR axis, clockwise according to L.
rotR = morph (rotateMove R) -- ^ 1/4 turn around LR axis, clockwise according to R.
rotB = morph (rotateMove B) -- ^ 1/4 turn around FB axis, clockwise according to B.
rotD = morph (rotateMove D) -- ^ 1/4 turn around UD axis, clockwise according to D.

rotUD = rotU . rotU -- ^ 1/2 turn around UD axis.
rotFB = rotF . rotF -- ^ 1/2 turn around FB axis.
rotLR = rotL . rotL -- ^ 1/2 turn around LR axis.

rotFR = rotFB . rotU -- ^ 1/2 turn around FR edge.
rotFL = rotU . rotFB -- ^ 1/2 turn around FL edge.
rotUL = rotUD . rotF -- ^ 1/2 turn around UL edge.
rotUR = rotF . rotUD -- ^ 1/2 turn around UR edge.
rotUB = rotUD . rotL -- ^ 1/2 turn around UB edge.
rotUF = rotL . rotUD -- ^ 1/2 turn around UF edge.

rotURF = rotF . rotU -- ^ 1/3 turn around diagonal.
rotUFR = rotD . rotB -- ^ 1/3 turn around diagonal.
rotUFL = rotU . rotF -- ^ 1/3 turn around diagonal.
rotULF = rotB . rotD -- ^ 1/3 turn around diagonal.
rotURB = rotF . rotD -- ^ 1/3 turn around diagonal.
rotUBR = rotU . rotB -- ^ 1/3 turn around diagonal.
rotUBL = rotD . rotF -- ^ 1/3 turn around diagonal.
rotULB = rotB . rotU -- ^ 1/3 turn around diagonal.

mirUD = morph (reflectMove U) -- ^ Reflection across UD axis.
mirFB = morph (reflectMove F) -- ^ Reflection across FB axis.
mirLR = morph (reflectMove L) -- ^ Reflection across LR axis.

rotMirU = rotU . mirUD -- ^ 1/4 turn around, and reflection across, UD axis, clockwise according to U.
rotMirF = rotF . mirFB -- ^ 1/4 turn around, and reflection across, FB axis, clockwise according to F.
rotMirL = rotL . mirLR -- ^ 1/4 turn around, and reflection across, LR axis, clockwise according to L.
rotMirR = rotR . mirLR -- ^ 1/4 turn around, and reflection across, UD axis, clockwise according to R.
rotMirB = rotB . mirFB -- ^ 1/4 turn around, and reflection across, FB axis, clockwise according to B.
rotMirD = rotD . mirUD -- ^ 1/4 turn around, and reflection across, LR axis, clockwise according to D.

mirUL = rotF . mirUD -- ^ Reflection across UL edge.
mirUR = mirUD . rotF -- ^ Reflection across UR edge.
mirUF = mirUD . rotL -- ^ Reflection across UF edge.
mirUB = rotL . mirUD -- ^ Reflection across UB edge.
mirFL = mirFB . rotU -- ^ Reflection across FL edge.
mirFR = rotU . mirFB -- ^ Reflection across FR edge.

rotMirUFL = rotURF . mirFB -- ^ 1/6 turn around, and reflection across, diagonal.
rotMirUBL = mirFB . rotURF -- ^ 1/6 turn around, and reflection across, diagonal.
rotMirULB = rotUFR . mirFB -- ^ 1/6 turn around, and reflection across, diagonal.
rotMirULF = mirFB . rotUFR -- ^ 1/6 turn around, and reflection across, diagonal.
rotMirURF = rotUBL . mirFB -- ^ 1/6 turn around, and reflection across, diagonal.
rotMirURB = mirFB . rotUBL -- ^ 1/6 turn around, and reflection across, diagonal.
rotMirUFR = mirFB . rotULB -- ^ 1/6 turn around, and reflection across, diagonal.
rotMirUBR = rotULB . mirFB -- ^ 1/6 turn around, and reflection across, diagonal.

mir = mirUD . mirFB . mirLR -- ^ Reflection across center of cube.
