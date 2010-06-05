{-# LANGUAGE ScopedTypeVariables, TypeSynonymInstances #-}
module Cube where

import Rubik
import Data.Array.IArray
import Data.Bits
import Data.Char

-- | The faces of the cube, named for the standard Rubik's Cube names
-- for the faces: up, down, left, right, front, back.  The order is
-- such that the opposite face for face X is the same distance from
-- the back of the list as X is from the front.
data Face = U | F | L | R | B | D
          deriving (Eq, Ord, Enum, Bounded, Read, Show, Ix)

faceToName :: Face -> Char
faceToName f = toLower c where [c] = show f

nameToFace :: Char -> Face
nameToFace name = read [toUpper name]

oppositeFaceNumber :: Int -> Int
oppositeFaceNumber = (5 -)

oppositeFace :: Face -> Face
oppositeFace = toEnum . oppositeFaceNumber . fromEnum

isOpposite f1 f2 = f1 == oppositeFace f2

facesIndex :: [Face] -> Int
facesIndex [] = 0
facesIndex (f:fs) = toBit f .|. facesIndex fs
  where toBit f = bit $ fromEnum f

-- | The faces that touch a given face, in clockwise order.
neighboringFaces :: Face -> [Face]
neighboringFaces = (neighbors !)
  where neighbors :: Array Face [Face]
        neighbors = listArray (minBound, maxBound) [nf i | i <- [0..5]]
        nf n
          | n < 3 = let firstTwo = map toEnum [(n+1)`mod`3, (n+2)`mod`3]
                    in firstTwo ++ map oppositeFace firstTwo
          | otherwise = reverse . nf . oppositeFaceNumber $ n

-- | The vertices of a given face, as length-3 lists of their faces.
-- The faces appear in clockwise order starting with the given face.
faceVertexTriples :: Face -> [[Face]]
faceVertexTriples f = vt ns $ tail $ cycle ns
  where ns = neighboringFaces f
        vt [] _ = []
        vt (x:xs) (y:ys) = [f, x, y] : vt xs ys

-- | All the vertices, as length-3 lists of their faces.  The faces
-- appear in clockwise order, starting with Up or Down.
vertices :: [[Face]]
vertices = faceVertexTriples U ++ faceVertexTriples D

instance Enum Vertex where
  toEnum i = if i < 0 || i >= length vertices then undefined else Vertex i
  fromEnum (Vertex v) = v
  
instance Bounded Vertex where
  minBound = Vertex 0
  maxBound = Vertex $ length vertices - 1

-- | The canonical name of a vertex piece.
vertexName :: Vertex -> String
vertexName = map faceToName . vertexFaces

-- | The 3 faces of a vertex piece, in canonical order.
vertexFaces :: Vertex -> [Face]
vertexFaces = \(Vertex v) -> faceArray ! v
  where faceArray :: Array Int [Face]
        faceArray = listArray (0, length vertices - 1) vertices

-- | Converts a string containing 3 face names into the corresponding
-- vertex.
nameToVertex :: String -> Vertex
nameToVertex = facesToVertex . map nameToFace

-- | Converts a list of 3 faces into the corresponding vertex.
facesToVertex :: [Face] -> Vertex
facesToVertex = (vertexArray !) . facesIndex
  where vertexArray :: Array Int Vertex
        vertexArray = array (0, 1`shiftL`6 - 1)
                      [(i, v) | v <- map Vertex [0 .. length vertices - 1],
                       let i = facesIndex $ vertexFaces v]

-- | The vertices that belong to a given face, in clockwise order.
faceVertices :: Face -> [Vertex]
faceVertices = (verticesArray !)
  where verticesArray :: Array Face [Vertex]
        verticesArray = listArray (minBound, maxBound)
                        [vs f | f <- [minBound..]]
        vs = map facesToVertex . faceVertexTriples


-- | All the edges, as length-2 lists of faces.  The first face is the
-- "distinguished" one, Up or Down if applicable, or Front or Back
-- otherwise.
edges :: [[Face]]
edges = topEdges ++ frontEdges ++ backEdges ++ bottomEdges
  where topEdges = map (\f -> [U, f]) $ neighboringFaces U
        frontEdges = [[F, L], [F, R]]
        backEdges = invert frontEdges
        bottomEdges = invert topEdges
        invert = reverse . map (map oppositeFace)

instance Enum Edge where
  toEnum i = if i < 0 || i >= length edges then undefined else Edge i
  fromEnum (Edge v) = v
  
instance Bounded Edge where
  minBound = Edge 0
  maxBound = Edge $ length edges - 1

-- | The canonical name of an edge piece.
edgeName :: Edge -> String
edgeName = map faceToName . edgeFaces
  
-- | The 2 faces of an edge piece, in canonical order.
edgeFaces :: Edge -> [Face]
edgeFaces = \(Edge e) -> faceArray ! e
  where faceArray :: Array Int [Face]
        faceArray = listArray (0, length edges - 1) edges
  
-- | Converts a string containing 2 face names into the corresponding
-- edge.
nameToEdge :: String -> Edge
nameToEdge = facesToEdge . map nameToFace

-- | Converts a pair of faces into the corresponding edge.
facesToEdge :: [Face] -> Edge
facesToEdge = (edgeArray !) . facesIndex
  where edgeArray :: Array Int Edge
        edgeArray = array (0, 1`shiftL`6 - 1)
                      [(i, v) | v <- map Edge [0 .. length edges - 1],
                       let i = facesIndex $ edgeFaces v]

-- | The edges that belong to a given face, in clockwise order.
faceEdges :: Face -> [Edge]
faceEdges = (edgesArray !)
  where edgesArray :: Array Face [Edge]
        edgesArray = listArray (minBound, maxBound)
                     [es f | f <- [minBound..]]
        es = map facesToEdge . efs
        efs f = map (\f2 -> [f, f2]) (neighboringFaces f)
