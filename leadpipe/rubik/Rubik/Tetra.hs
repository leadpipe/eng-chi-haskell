{-# LANGUAGE TypeFamilies, TypeSynonymInstances #-}

-- | Describes a tetrahedron in terms of faces, edges, and vertices.
module Rubik.Tetra where

import Rubik.Algebra
import Rubik.Cycles
import Rubik.Polyhedron
import Rubik.Puzzle

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

  allEdgesAsFaces = faceEdgesAsFaces F ++ [[L, R], [R, D], [D, L]]

  allVerticesAsFaces = faceNeighborTriples F ++ [[D, R, L]]


data FaceTwist = FaceTwist Face Twist3 deriving (Eq, Ord)

instance PuzzleMove FaceTwist where
  undoMove (FaceTwist f t) = FaceTwist f (-t)

  joinMoves m1@(FaceTwist f1 t1) m2@(FaceTwist f2 t2)
    | f1 == f2           = let t = t1 + t2 in
                           if t == 0 then [] else [FaceTwist f1 t]
    | otherwise          = [m1, m2]

  isTrivialMove (FaceTwist _ t) = t == 0

instance Show FaceTwist where
  showsPrec _ (FaceTwist f t) = shows f . shows t

instance Read FaceTwist where
  readsPrec _ "" = []
  readsPrec _ (c:s) = maybeToList $ do
    f <- nameToMaybeFace c
    (t, s') <- listToMaybe (reads s)
    if t == 0 then fail "no twist specified"
      else return (FaceTwist f t, s')

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
