{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses, ScopedTypeVariables #-}

-- | Defines the relationships among the faces, edges, and vertices of
-- a polyhedron.
module Rubik.Geometry where

import Data.Array.IArray ((!), Array, Ix, array, listArray)
import Data.Bits ((.|.), bit, shiftL)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (inits, tails)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, maybeToList)


-- | A class that relates the faces, edges, and vertices of a
-- polyhedron used as the basis for a Rubik-style puzzle.
class (Enum f, Bounded f, Ix f, Enum e, Bounded e, Enum v, Bounded v)
      => Polyhedron f e v | f -> e, e -> v, v -> f where

  -- | Every polyhedron must have single-character names for its
  -- faces.  This list relates them.
  faceNames :: [(f, Char)]

  -- | A face's single-character name.
  faceToName :: f -> Char
  faceToName = (nameArray !)
    where nameArray :: Array f Char
          nameArray = array (minBound, maxBound) faceNames

  -- | Converts back from a face's name to the face.
  nameToFace :: Char -> f
  nameToFace = fromJust . nameToMaybeFace

  -- | Converts back from a face's name to the face, if it is one.
  nameToMaybeFace :: Char -> Maybe f
  nameToMaybeFace = flip Map.lookup faceMap
    where faceMap :: Map Char f
          faceMap = Map.fromList $ map (\(f, c) -> (c, f)) faceNames

  -- | Converts a list of faces into an int, as a bit set.
  facesIndex :: [f] -> Int
  facesIndex [] = 0
  facesIndex (f:fs) = toBit f .|. facesIndex fs
    where toBit f = bit $ fromEnum f - fromEnum (minBound::f)

  -- | The faces that touch a given face, in clockwise order.
  neighboringFaces :: f -> [f]


  -- | The edges of a given face, as length-2 lists of their faces.
  -- The given face appears first.
  faceEdgePairs :: f -> [[f]]
  faceEdgePairs f = map (\f2 -> [f, f2]) $ neighboringFaces f

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
  facesToEdge = fromJust . facesToMaybeEdge

  -- | Converts a pair of faces into the corresponding edge, if there
  -- is one.
  facesToMaybeEdge :: [f] -> Maybe e
  facesToMaybeEdge = flip IntMap.lookup edgeMap . facesIndex
    where edgeMap = makeFaceBitsetMap edgeFaces

  -- | The edges that belong to a given face, in clockwise order.
  faceEdges :: f -> [e]
  faceEdges = (edgesArray !)
    where edgesArray :: Array f [e]
          edgesArray = listArray (minBound, maxBound)
                       [es f | f <- [minBound..]]
          es :: f -> [e]
          es = map facesToEdge . efs
          efs f = map (\f2 -> [f, f2]) (neighboringFaces f)


  -- | The vertices of a given face, as lists of the faces that meet
  -- at each vertex.  The faces for each vertex appear in clockwise
  -- order starting with the given face.  And the vertices
  -- corresponding to the lists of faces are also in clockwise order.
  faceVerticesAsFaces :: f -> [[f]]
  faceVerticesAsFaces f = map vfs $ neighboringFaces f
    where vfs :: f -> [f]
          vfs f2 = (Map.!) facesMap (f, f2)
          facesMap :: Map (f, f) [f]
          facesMap = Map.fromList $ concatMap fvs allVerticesAsFaces
          fvs :: [f] -> [((f, f), [f])]
          fvs = map indexFaces . allRotations
          allRotations :: [f] -> [[f]]
          allRotations fs = map (uncurry (++)) $ tail $ zip (tails fs) (inits fs)
          indexFaces :: [f] -> ((f, f), [f])
          indexFaces fs@(f1:f2:_) = ((f1, f2), fs)

  -- | All the vertices, as lists of their faces.  The faces must
  -- appear in clockwise order, starting with the vertex's
  -- distinguished face.
  allVerticesAsFaces :: [[f]]

  -- | The canonical name of a vertex piece.
  vertexName :: v -> String
  vertexName = map faceToName . vertexFaces

  -- | The faces of a vertex piece, in canonical order.
  vertexFaces :: v -> [f]
  vertexFaces = (facesArray !) . fromEnum
    where facesArray = makeFacesArray allVerticesAsFaces

  -- | Converts a string containing face names into the corresponding
  -- vertex.
  nameToVertex :: String -> v
  nameToVertex = facesToVertex . map nameToFace

  -- | Converts a list of faces into the corresponding vertex.
  facesToVertex :: [f] -> v
  facesToVertex = fromJust . facesToMaybeVertex

  -- | Converts a list of faces into the corresponding vertex, if
  -- there is one.
  facesToMaybeVertex :: [f] -> Maybe v
  facesToMaybeVertex = flip IntMap.lookup vertexMap . facesIndex
    where vertexMap = makeFaceBitsetMap vertexFaces

  -- | The vertices that belong to a given face, in clockwise order.
  faceVertices :: f -> [v]
  faceVertices = (verticesArray !)
    where verticesArray :: Array f [v]
          verticesArray = listArray (minBound, maxBound)
                          [vs f | f <- [minBound..]]
          vs :: f -> [v]
          vs = map facesToVertex . faceVerticesAsFaces


-- | A helper to implement edgeFaces and vertexFaces.  Makes an array
-- from a list of lists of faces, such as allVerticesAsFaces and
-- allEdgesAsFaces.
makeFacesArray :: [[f]] -> Array Int [f]
makeFacesArray fss = listArray (0, length fss - 1) fss

-- | A helper to implement facesToMaybeEdge and facesToMaybeVertex.
-- Makes a map that maps a bit-set of face numbers to either edges or
-- vertices.
makeFaceBitsetMap :: forall a f e v. (Enum a, Bounded a, Polyhedron f e v) =>
                     (a -> [f]) -> IntMap a
makeFaceBitsetMap xxFaces =
  IntMap.fromList [(i, a) | a <- [minBound..], let i = facesIndex $ xxFaces a]

-- | A helper to implement the toEnum methods of edge and vertex
-- types.
toBoundedEnum :: forall a. (Bounded a, Enum a) => (Int -> a) -> Int -> a
toBoundedEnum ctor i = if i < fromEnum (minBound::a) || i > fromEnum (maxBound::a)
                       then undefined
                       else ctor i

-- | A helper to implement allVerticesAsFaces for polyhedra with 3
-- faces per vertex.
faceNeighborTriples :: forall f e v. (Polyhedron f e v) => f -> [[f]]
faceNeighborTriples f = vt ns $ tail $ cycle ns
  where ns = neighboringFaces f
        vt [] _ = []
        vt (x:xs) (y:ys) = [f, x, y] : vt xs ys


-- | A helper to implement Read for face types.
readSFace :: forall f e v. (Polyhedron f e v) => ReadS f
readSFace (c:cs) = maybeToList $ do
  f <- nameToMaybeFace c
  return (f, cs)
readSFace _ = []

-- | A helper to implement Read for edge types.
readSEdge :: forall f e v. (Polyhedron f e v) => ReadS e
readSEdge (c1:c2:cs) = maybeToList $ do
  f1 <- nameToMaybeFace c1
  f2 <- nameToMaybeFace c2
  e <- facesToMaybeEdge [f1, f2]
  return (e, cs)
readSEdge _ = []

-- | A helper to implement Read for vertex types.
readSVertex :: forall f e v. (Polyhedron f e v) => ReadS v
readSVertex (c1:c2:c3:cs) = maybeToList $ do
  f1 <- nameToMaybeFace c1
  f2 <- nameToMaybeFace c2
  f3 <- nameToMaybeFace c3
  v <- facesToMaybeVertex [f1, f2, f3]
  return (v, cs)
readSVertex _ = []

-- | Rotates a list by n places.
rotate n xs = drop n xs ++ take n xs
