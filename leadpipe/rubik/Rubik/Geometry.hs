{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses, ScopedTypeVariables #-}

-- | Defines the relationships among the faces, edges, and vertices of
-- a polyhedron.
module Rubik.Geometry where

import Data.Array.IArray
import Data.Bits
import Data.Char


-- | A class that relates the faces, edges, and vertices of a
-- polyhedron used as the basis for a Rubik-style puzzle.
class (Enum f, Bounded f, Ix f, Enum e, Bounded e, Enum v, Bounded v)
      => Polyhedron f e v | f -> e, e -> v, v -> f where

  -- | Every polyhedron must have single-character names for its
  -- faces.
  faceToName :: f -> Char

  -- | Converts back from a face's name to the face.
  nameToFace :: Char -> f

  -- | Converts a list of faces into an integer, as a bit set.
  facesIndex :: [f] -> Integer
  facesIndex [] = 0
  facesIndex (f:fs) = toBit f .|. facesIndex fs
    where toBit f = bit $ fromEnum f - fromEnum (minBound::f)

  -- | The faces that touch a given face, in clockwise order.
  neighboringFaces :: f -> [f]

  -- | The vertices of a given face, as length-3 lists of their faces.
  -- The faces appear in clockwise order starting with the given face.
  faceVertexTriples :: f -> [[f]]
  faceVertexTriples f = vt ns $ tail $ cycle ns
    where ns = neighboringFaces f
          vt [] _ = []
          vt (x:xs) (y:ys) = [f, x, y] : vt xs ys

  -- | All the vertices, as length-3 lists of their faces.  The faces
  -- must appear in clockwise order, starting with the vertex's
  -- distinguished face.
  allVerticesAsFaces :: [[f]]

  -- | The canonical name of a vertex piece.
  vertexName :: v -> String
  vertexName = map faceToName . vertexFaces

  -- | The 3 faces of a vertex piece, in canonical order.
  vertexFaces :: v -> [f]
  vertexFaces = (facesArray !) . fromEnum
    where facesArray = makeFacesArray allVerticesAsFaces

  -- | Converts a string containing 3 face names into the
  -- corresponding vertex.
  nameToVertex :: String -> v
  nameToVertex = facesToVertex . map nameToFace

  -- | Converts a list of 3 faces into the corresponding vertex.
  facesToVertex :: [f] -> v
  facesToVertex = (vertexArray !) . facesIndex
    where vertexArray = makeFaceBitsetArray vertexFaces

  -- | The vertices that belong to a given face, in clockwise order.
  faceVertices :: f -> [v]
  faceVertices = (verticesArray !)
    where verticesArray :: Array f [v]
          verticesArray = listArray (minBound, maxBound)
                          [vs f | f <- [minBound..]]
          vs :: f -> [v]
          vs = map facesToVertex . faceVertexTriples


  -- | All the edges, as length-2 lists of faces with the
  -- distinguished one first.
  allEdgesAsFaces :: [[f]]

  -- | The canonical name of an edge piece.
  edgeName :: e -> String
  edgeName = map faceToName . edgeFaces
  
  -- | The 2 faces of an edge piece, in canonical order.
  edgeFaces :: e -> [f]
  edgeFaces = (facesArray !) . fromEnum
    where facesArray = makeFacesArray allEdgesAsFaces
  
  -- | Converts a string containing 2 face names into the
  -- corresponding edge.
  nameToEdge :: String -> e
  nameToEdge = facesToEdge . map nameToFace

  -- | Converts a pair of faces into the corresponding edge.
  facesToEdge :: [f] -> e
  facesToEdge = (edgeArray !) . facesIndex
    where edgeArray = makeFaceBitsetArray edgeFaces

  -- | The edges that belong to a given face, in clockwise order.
  faceEdges :: f -> [e]
  faceEdges = (edgesArray !)
    where edgesArray :: Array f [e]
          edgesArray = listArray (minBound, maxBound)
                       [es f | f <- [minBound..]]
          es :: f -> [e]
          es = map facesToEdge . efs
          efs f = map (\f2 -> [f, f2]) (neighboringFaces f)


-- | A helper to implement vertexFaces and edgeFaces.  Makes an array
-- from a list of lists of faces, such as allVerticesAsFaces and
-- allEdgesAsFaces.
makeFacesArray :: [[f]] -> Array Int [f]
makeFacesArray fss = listArray (0, length fss - 1) fss

-- | A helper to implement facesToVertex and facesToEdge.  Makes an
-- array that maps a bit-set of face numbers to either edges or
-- vertices.
makeFaceBitsetArray :: forall a f e v. (Enum a, Bounded a, Polyhedron f e v) =>
                       (a -> [f]) -> Array Integer a
makeFaceBitsetArray xxFaces =
  array (0, 1`shiftL`numFaces - 1) [(i, a) | a <- [minBound..],
                                    let i = facesIndex $ xxFaces a]
    where numFaces = fromEnum (maxBound::f) - fromEnum (minBound::f)
