{-# LANGUAGE MultiParamTypeClasses, ScopedTypeVariables, TypeSynonymInstances #-}
module Rubik.Dodeca where

import Rubik.Geometry

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

-- | The edges of the dodecahedron.
newtype Edge = Edge Int deriving (Eq, Ord, Ix)

-- | The vertices of the dodecahedron.
newtype Vertex = Vertex Int deriving (Eq, Ord, Ix)


oppositeFaceNumber :: Int -> Int
oppositeFaceNumber = (11 -)

oppositeFace :: Face -> Face
oppositeFace = toEnum . oppositeFaceNumber . fromEnum

isOpposite f1 f2 = f1 == oppositeFace f2

-- | The distinguished faces for dodecahedron edges are: the north or
-- south polar face, for the ten polar edges; and the faces which
-- would move the edge to a polar face if the face were rotated
-- clockwise once or twice, for the ten vertical and ten equatorial
-- edges.
allEdgesAsFaces' = polarEdges ++ verticalEdges ++ equatorialEdges
  where polarEdges = faceEdgePairs An ++ faceEdgePairs As
        verticalEdges = verticalEdgePairs An ++ verticalEdgePairs As
        equatorialEdges = equatorialEdgePairs An ++ equatorialEdgePairs As
        verticalEdgePairs f = transpose [fs, rotate 1 fs]
          where fs = neighboringFaces f
        equatorialEdgePairs f = transpose [fs, map oppositeFace $ rotate 3 fs]
          where fs = neighboringFaces f

-- | The distinguished faces for dodecahedron vertices are: the north
-- or south polar face, for the ten polar vertices; and the face which
-- would move the vertex to a polar face if the face were rotated
-- clockwise, for the ten equatorial vertices.
allVerticesAsFaces' = polarVertices ++ equatorialVertices
  where polarVertices = faceVertexTriples An ++ faceVertexTriples As
        equatorialVertices = equatorialVertexTriples An
                             ++ equatorialVertexTriples As
        equatorialVertexTriples f = transpose [fs1, fs2, fs3]
          where fs1 = neighboringFaces f
                fs2 = map oppositeFace $ rotate 3 fs1
                fs3 = rotate 1 fs1


instance Polyhedron Face Edge Vertex where

  faceNames = [(An, 'a'), (Bn, 'b'), (Cn, 'c'), (Dn, 'd'), (En, 'e'), (Fn, 'f'),
               (As, 'A'), (Bs, 'B'), (Cs, 'C'), (Ds, 'D'), (Es, 'E'), (Fs, 'F')]

  neighboringFaces = (neighbors !)
    where neighbors :: Array Face [Face]
          neighbors = listArray (minBound, maxBound) [nf i | i <- [0..11]]
          nf n
            | n == 0 = [Bn, Cn, Dn, En, Fn]
            | n < 6 = let f m = toEnum (1 + (n+m-1)`mod`5)
                          g = oppositeFace . f
                      in [An, f(-1), g 2, g(-2), f 1]
            | otherwise = map oppositeFace $ reverse . nf . oppositeFaceNumber $ n

  allVerticesAsFaces = allVerticesAsFaces'

  allEdgesAsFaces = allEdgesAsFaces'


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
  maxBound = Edge $ length allEdgesAsFaces' - 1

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
  maxBound = Vertex $ length allVerticesAsFaces' - 1

instance Show Vertex where
  showsPrec _ = showString . vertexName

instance Read Vertex where
  readsPrec _ = readSVertex
