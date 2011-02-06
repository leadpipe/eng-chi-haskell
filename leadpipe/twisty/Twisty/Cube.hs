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

{-# LANGUAGE PatternGuards, TypeFamilies, TypeSynonymInstances #-}

-- | Describes a cube in terms of faces, edges, and vertices.
module Twisty.Cube where

import Twisty.Cycles
import Twisty.Group
import qualified Twisty.Memo as Memo
import Twisty.Polyhedron
import Twisty.Puzzle
import Twisty.Twists
import Twisty.Wreath

import Data.Ix (Ix)
import Data.Maybe (listToMaybe, maybeToList)
import GHC.Enum (boundedEnumFrom, boundedEnumFromThen)

-- | The faces of the cube.  The order is such that the opposite face
-- for face X is the same distance from the back of the list as X is
-- from the front.  (The names stand for up, front, left, right, back,
-- and down.)
data Face = U | F | L | R | B | D
          deriving (Eq, Ord, Enum, Bounded, Ix)

oppositeFaceNumber :: Int -> Int
oppositeFaceNumber = (5 -)

oppositeFace :: Face -> Face
oppositeFace = toEnum . oppositeFaceNumber . fromEnum

isOpposite f1 f2 = f1 == oppositeFace f2

instance PolyFace Face where

  newtype PolyEdge Face = Edge Int deriving (Eq, Ord, Ix)
  newtype PolyVertex Face = Vertex Int deriving (Eq, Ord, Ix)

  faceNames = [(U, 'u'), (F, 'f'), (L, 'l'),
               (R, 'r'), (B, 'b'), (D, 'd')]

  neighboringFaces = Memo.array nf
    where nf :: Face -> [Face]
          nf f
            | f < R = let n = fromEnum f
                          firstTwo = map toEnum [(n+1)`mod`3, (n+2)`mod`3]
                      in firstTwo ++ map oppositeFace firstTwo
            | otherwise = reverse . nf . oppositeFace $ f

  allEdgesAsFaces = topEdges ++ middleEdges ++ bottomEdges
    where topEdges = faceEdgesAsFaces U
          middleEdges = [[F, L], [F, R], [B, R], [B, L]]
          bottomEdges = faceEdgesAsFaces D

  allVerticesAsFaces = faceNeighborTriples U ++ faceNeighborTriples D


data FaceTwist = FaceTwist Face Twist4 deriving (Eq, Ord)

instance PuzzleMove FaceTwist where
  undoMove (FaceTwist f t) = FaceTwist f (-t)

  joinMoves m1@(FaceTwist f1 t1) m2@(FaceTwist f2 t2)
    | f1 == f2           = let t = t1 + t2 in
                           if t == 0 then [] else [FaceTwist f1 t]
    | f1 `isOpposite` f2 = [max m1 m2, min m1 m2]
    | otherwise          = [m1, m2]

  isTrivialMove (FaceTwist _ t) = t == 0

instance Show FaceTwist where
  showsPrec _ (FaceTwist f t) = shows f . shows t

instance Read FaceTwist where
  readsPrec _ "" = []
  readsPrec _ (c:s) = maybeToList $ do
    f <- nameToMaybeFace c
    (t, s') <- listToMaybe (reads s)
    return (FaceTwist f t, s')

type Edge = PolyEdge Face
type Vertex = PolyVertex Face

instance WreathPermutable Edge where
  type WreathTwist Edge = Flip

instance WreathPermutable Vertex where
  type WreathTwist Vertex = Twist3

instance Show Face where
  showsPrec _ = showChar . faceToName

instance Read Face where
  readsPrec _ = readSFace

instance Enum Edge where
  toEnum = toBoundedEnum Edge
  fromEnum (Edge e) = e
  enumFrom = boundedEnumFrom
  enumFromThen = boundedEnumFromThen

instance Bounded Edge where
  minBound = Edge 0
  maxBound = Edge $ length (allEdgesAsFaces::[[Face]]) - 1

instance Show Edge where
  showsPrec _ = showString . edgeName

instance Read Edge where
  readsPrec _ = readSEdge

instance Enum Vertex where
  toEnum = toBoundedEnum Vertex
  fromEnum (Vertex v) = v
  enumFrom = boundedEnumFrom
  enumFromThen = boundedEnumFromThen

instance Bounded Vertex where
  minBound = Vertex 0
  maxBound = Vertex $ length (allVerticesAsFaces::[[Face]]) - 1

instance Show Vertex where
  showsPrec _ = showString . vertexName

instance Read Vertex where
  readsPrec _ = readSVertex
