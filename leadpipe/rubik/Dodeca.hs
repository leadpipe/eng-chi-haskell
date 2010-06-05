{-# LANGUAGE ScopedTypeVariables, TypeSynonymInstances #-}

-- | Common definitions for Rubik-style dodecahedron puzzles.
module Dodeca where

import Rubik
import Data.Array.IArray
import Data.Char

-- | The faces of the dodecahedron.  The north polar face is An, and
-- the remaining northern hemisphere faces are Xn, X in (B..F).  The
-- opposite face to Xn is Xs.  The order is such that the opposite
-- face for face X is the same distance from the back of the list as X
-- is from the front.
data Face = An | Bn | Cn | Dn | En | Fn | Fs | Es | Ds | Cs | Bs | As
          deriving (Eq, Ord, Enum, Bounded, Read, Show, Ix)

-- | Converts each face to a character.  By perverse convention, the
-- northern hemisphere faces are lower case and the southern are upper
-- case.
faceToName :: Face -> Char
faceToName f = name $ show f
  where name [x, h]
          | h == 'n' = toLower x
          | h == 's' = x

nameToFace :: Char -> Face
nameToFace name = read [toUpper name, toHemisphere name]
  where toHemisphere c = if c == toUpper c then 'n' else 's'

oppositeFaceNumber :: Int -> Int
oppositeFaceNumber = (11 -)

oppositeFace :: Face -> Face
oppositeFace = toEnum . oppositeFaceNumber . fromEnum

isOpposite f1 f2 = f1 == oppositeFace f2

-- | The faces that touch a given face, in clockwise order.
neighboringFaces :: Face -> [Face]
neighboringFaces = (neighbors !)
  where neighbors :: Array Face [Face]
        neighbors = listArray (minBound, maxBound) [nf i | i <- [0..11]]
        nf n
          | n == 0 = [Bn, Cn, Dn, En, Fn]
          | n < 6 = let f m = toEnum (1 + (n+m-1)`mod`5)
                        g = oppositeFace . f
                    in [An, f(-1), g 2, g(-2), f 1]
          | otherwise = map oppositeFace $ reverse . nf . oppositeFaceNumber $ n

-- | All the vertices, as triples of faces.  The faces appear in
-- clockwise order, starting with a distinguished face: either north
-- or south polar face, for polar vertices, or the face which would
-- move the vertex to a polar face if the face were rotated clockwise,
-- for equatorial vertices.
vertices :: [(Face, Face, Face)]
vertices = topVertices ++ northVertices ++ southVertices ++ bottomVertices
  where topVertices = zip3 (repeat An) topNeighbors rotatedTopNeighbors
        topNeighbors = neighboringFaces An
        rotatedTopNeighbors = tail topNeighbors ++ [head topNeighbors]
        northVertices = zip3 topNeighbors centerFaces rotatedTopNeighbors
        centerFaces = drop 2 $ cycle $ map oppositeFace rotatedTopNeighbors
        southVertices = invert northVertices
        bottomVertices = invert topVertices
        invert vs = reverse $ map opp vs
        opp (f, g, h) = (oppositeFace f, oppositeFace h, oppositeFace g)

instance Enum Vertex where
  toEnum i = if i < 0 || i >= length vertices then undefined else Vertex i
  fromEnum (Vertex v) = v
  
instance Bounded Vertex where
  minBound = Vertex 0
  maxBound = Vertex $ length vertices - 1

-- | The canonical name of a vertex piece.
vertexName :: Vertex -> String
vertexName v = let (a, b, c) = vertexFaces v
               in map faceToName [a, b, c]

-- | The 3 faces of a vertex piece, in canonical order.
vertexFaces :: Vertex -> (Face, Face, Face)
vertexFaces = \(Vertex v) -> faceArray ! v
  where faceArray :: Array Int (Face, Face, Face)
        faceArray = listArray (0, length vertices - 1) vertices
{-
-- | The canonical name of a vertex piece.
vertexName :: Int -> String
vertexName v = let (a, b, c) = vertexFaces v
               in map faceToName [a, b, c]

-- | The 3 faces of a vertex piece, in canonical order.
vertexFaces :: Int -> (Int, Int, Int)
vertexFaces v = 

-- | The vertices that belong to a given face, in clockwise order.
vertices :: Int -> [Int]
vertices f = 


-- | The canonical name of an edge piece.
edgeName :: Int -> String
edgeName e = let (a, b) = edgeFaces e in
  map faceToName [a, b]
  
-- | The 2 faces of an edge piece, in canonical order.
edgeFaces :: Int -> (Int, Int)
edgeFaces e = 
  
-- | The edges that belong to a given face, in clockwise order.
edges :: Int -> [Int]
edges f = 

-}
