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

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Describes a tetrahedron in terms of faces, edges, and vertices.
module Twisty.Tetra where

import Twisty.FaceTwist
import Twisty.Group
import Twisty.Polyhedron
import Twisty.Puzzle
import Twisty.Twists
import Twisty.Wreath
import Twisty.Zn

import Data.Ix (Ix)
import Data.Maybe (listToMaybe, maybeToList)
import GHC.Enum (boundedEnumFrom, boundedEnumFromThen)


data Face = F | L | R | D deriving (Eq, Ord, Enum, Bounded, Ix)

instance PolyFace Face where

  newtype PolyEdge Face = Edge Int deriving (Eq, Ord, Ix)
  newtype PolyVertex Face = Vertex Int deriving (Eq, Ord, Ix)

  faceNames = [(F, 'f'), (L, 'l'), (R, 'r'), (D, 'd')]

  neighboringFaces = nf
    where nf F = [L, R, D]
          nf L = [F, D, R]
          nf R = [D, F, L]
          nf D = [R, L, F]

  allEdgesAsFaces = [[D, F], [D, L], [D, R], [F, L], [L, R], [R, F]]
--  allEdgesAsFaces = [[F, D], [F, L], [F, R], [L, D], [D, R], [R, L]]
--  allEdgesAsFaces = [[F, D], [L, F], [R, F], [D, L], [D, R], [R, L]]
--  allEdgesAsFaces = [[F, L], [L, R], [R, F], [D, R], [D, F], [L, D]]

  allVerticesAsFaces = [[L, R, F], [R, L, D], [F, D, L], [D, F, R]]


-- | The generic tetra move type.
type TetraMove depth = FaceTwist Face depth Twist3

-- | Single-layer tetra moves: for 3x3 tetrahedra.  These show as just the face
-- and the twist.
type TetraMove1 = TetraMove Z1

instance Show TetraMove1 where
  showsPrec _ (FaceTwist f _ t) = shows f . shows t

instance Read TetraMove1 where
  readsPrec _ "" = []
  readsPrec _ (c:s) = maybeToList $ do
    f <- nameToMaybeFace c
    (t, s') <- listToMaybe (reads s)
    return (FaceTwist f 0 t, s')

-- | The generic twist accumulator for tetras.
type TetraTwists depth = CumulativeTwists Face depth Twist3

type TetraTwists1 = TetraTwists Z1


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
