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
{-# LANGUAGE TypeSynonymInstances #-}

-- | Describes a cube in terms of faces, edges, and vertices.
module Twisty.Cube where

import Twisty.Cycles
import Twisty.FaceTwist
import Twisty.Group
import qualified Twisty.Memo as Memo
import Twisty.Polyhedron
import Twisty.Puzzle
import Twisty.Twists
import Twisty.Wreath
import Twisty.Zn

import Data.Char (isUpper, toLower, toUpper)
import Data.Ix (Ix)
import Data.Maybe (listToMaybe, maybeToList)
import GHC.Enum (boundedEnumFrom, boundedEnumFromThen)

-- | The faces of the cube.  The order is such that the opposite face
-- for face X is the same distance from the back of the list as X is
-- from the front.
data Face = U  -- ^ The \"up\" (top) face.
          | F  -- ^ The front face.
          | L  -- ^ The left face.
          | R  -- ^ The right face.
          | B  -- ^ The back face.
          | D  -- ^ The \"down\" (bottom) face.
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


-- | The generic cube move type.
type CubeMove depth = FaceTwist Face Twist4 depth

-- | Single-layer cube moves: for 2x2, 3x3 cubes.  These show as just the face
-- and the twist.
type CubeMove1 = CubeMove Z1

-- | Double-layer cube moves: for 4x4, 5x5 cubes.  These use upper case for the
-- face name to indicate moving both layers, and lower case for just the outer
-- layer.
type CubeMove2 = CubeMove Z2

-- | Triple-layer cube moves: for 6x6, 7x7 cubes.  These use an upper case face
-- name followed by a caret to mean moving all 3 layers; upper case for the
-- outer 2 layers; and lower case for the outer layer.
type CubeMove3 = CubeMove Z3

instance Show CubeMove1 where
  showsPrec _ (FaceTwist f t _) = shows f . shows t

instance Read CubeMove1 where
  readsPrec _ "" = []
  readsPrec _ (c:s) = maybeToList $ do
    f <- nameToMaybeFace c
    (t, s') <- listToMaybe (reads s)
    return (FaceTwist f t 0, s')

instance Show CubeMove2 where
  showsPrec _ (FaceTwist f t d) = (if d == 0 then shows else showChar.toUpper.faceToName) f . shows t

instance Read CubeMove2 where
  readsPrec _ "" = []
  readsPrec _ (c:s) = maybeToList $ do
    f <- nameToMaybeFace (toLower c)
    (t, s') <- listToMaybe (reads s)
    return (FaceTwist f t (if isUpper c then 1 else 0), s')

instance Show CubeMove3 where
  showsPrec _ (FaceTwist f t d) = showFace d f . shows t
    where showFace 0 = shows
          showFace 1 = showChar . toUpper . faceToName
          showFace 2 = \f -> showFace 1 f . showChar '^'

instance Read CubeMove3 where
  readsPrec _ "" = []
  readsPrec _ (c:s) = maybeToList $ do
    f <- nameToMaybeFace (toLower c)
    let (d, s') = readDepth (isUpper c) s
    (t, s'') <- listToMaybe (reads s')
    return (FaceTwist f t d, s'')
      where readDepth upper s =
              if upper && not (null s) && head s == '^' then (2, tail s)
              else (if upper then 1 else 0, s)


-- | The generic twist accumulator for cubes.
type CubeTwists depth = CumulativeTwists Face Twist4 depth

type CubeTwists1 = CubeTwists Z1
type CubeTwists2 = CubeTwists Z2
type CubeTwists3 = CubeTwists Z3


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
