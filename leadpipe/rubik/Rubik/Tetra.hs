{-# LANGUAGE TypeFamilies, TypeSynonymInstances #-}

-- | Describes a tetrahedron in terms of faces, edges, and vertices.
module Rubik.Tetra where

import Rubik.Algebra
import Rubik.Cycles
import Rubik.Polyhedron

import Data.Ix (Ix)
import GHC.Enum (boundedEnumFrom, boundedEnumFromThen)


data Face = A | B | C | D deriving (Eq, Ord, Enum, Bounded, Ix)

instance PolyFace Face where

  newtype PolyEdge Face = Edge Int deriving (Eq, Ord, Ix)
  newtype PolyVertex Face = Vertex Int deriving (Eq, Ord, Ix)

  faceNames = [(A, 'a'), (B, 'b'), (C, 'c'), (D, 'd')]

  neighboringFaces = nf
    where nf A = [B, C, D]
          nf B = [A, D, C]
          nf C = [D, A, B]
          nf D = [C, B, A]

  allEdgesAsFaces = faceEdgesAsFaces A ++ [[B, C], [C, D], [D, B]]

  allVerticesAsFaces = faceNeighborTriples A ++ [[D, C, B]]


type Edge = PolyEdge Face
type Vertex = PolyVertex Face

type EdgeWreath = Wreath Flip
type VertexWreath = Wreath Twist3

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
