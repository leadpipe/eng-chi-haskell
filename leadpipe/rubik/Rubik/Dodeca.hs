{-# LANGUAGE TypeFamilies, TypeSynonymInstances #-}

-- | Describes a dodecahedron in terms of faces, edges, and vertices.
module Rubik.Dodeca where

import Rubik.Algebra
import Rubik.Cycles
import Rubik.Lists
import qualified Rubik.Memo as Memo
import Rubik.Polyhedron

import Data.Array.IArray (Ix, (!), Array, listArray)
import Data.List (transpose)
import GHC.Enum (boundedEnumFrom, boundedEnumFromThen)

-- | The faces of the dodecahedron.  The north polar face is An, and
-- the remaining northern hemisphere faces are Xn, X in (B..F).  The
-- opposite face to Xn is Xs.  The order is such that the opposite
-- face for face X is the same distance from the back of the list as X
-- is from the front.
data Face = An | Bn | Cn | Dn | En | Fn | Fs | Es | Ds | Cs | Bs | As
          deriving (Eq, Ord, Enum, Bounded, Ix)

oppositeFaceNumber :: Int -> Int
oppositeFaceNumber = (11 -)

oppositeFace :: Face -> Face
oppositeFace = toEnum . oppositeFaceNumber . fromEnum

isOpposite f1 f2 = f1 == oppositeFace f2

instance PolyFace Face where

  newtype PolyEdge Face = Edge Int deriving (Eq, Ord, Ix)
  newtype PolyVertex Face = Vertex Int deriving (Eq, Ord, Ix)

  faceNames = [(An, 'A'), (Bn, 'B'), (Cn, 'C'), (Dn, 'D'), (En, 'E'), (Fn, 'F'),
               (As, 'a'), (Bs, 'b'), (Cs, 'c'), (Ds, 'd'), (Es, 'e'), (Fs, 'f')]

  neighboringFaces = Memo.array nf
    where nf :: Face -> [Face]
          nf f
            | f == An = [Bn, Cn, Dn, En, Fn]
            | f <= Fn = let same n = toEnum (1 + (fromEnum f - 1 + n) `mod` 5)
                            opp = oppositeFace . same
                      in [An, same(-1), opp 2, opp(-2), same 1]
            | otherwise = map oppositeFace $ reverse . nf . oppositeFace $ f

  -- | The distinguished faces for dodecahedron edges are: the north or south
  -- polar face, for the ten polar edges; and the faces which would move the
  -- edge to a polar face if the face were rotated clockwise (north) or
  -- counterclockwise (south) once or twice, for the ten vertical and ten
  -- equatorial edges.
  allEdgesAsFaces = polarEdges ++ verticalEdges ++ equatorialEdges
    where polarEdges = faceEdgesAsFaces An ++ faceEdgesAsFaces As
          verticalEdges = northVerticalEdges ++ invert northVerticalEdges
          equatorialEdges = northEquatorialEdges ++ invert northEquatorialEdges
          northVerticalEdges = transpose [northNeighbors, rotate 1 northNeighbors]
          northEquatorialEdges = transpose [northNeighbors,
                                            map oppositeFace $ rotate 3 northNeighbors]
          northNeighbors = neighboringFaces An
          invert = map (map oppositeFace)

  -- | The distinguished faces for dodecahedron vertices are: the north or south
  -- polar face, for the ten polar vertices; and the face which would move the
  -- vertex to a polar face if the face were rotated clockwise (north) or
  -- counterclockwise (south), for the ten equatorial vertices.
  allVerticesAsFaces = polarVertices ++ equatorialVertices
    where polarVertices = faceNeighborTriples An ++ faceNeighborTriples As
          equatorialVertices = northEquatorialVertices ++ invert northEquatorialVertices
          northEquatorialVertices = transpose [fs1, fs2, fs3]
            where fs1 = neighboringFaces An
                  fs2 = map oppositeFace $ rotate 3 fs1
                  fs3 = rotate 1 fs1
          invert = map invertFaces
          invertFaces = swapFaces . map oppositeFace
          swapFaces [f1, f2, f3] = [f1, f3, f2]


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
