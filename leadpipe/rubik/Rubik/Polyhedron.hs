{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, TypeFamilies #-}

-- | Defines a type class for polyhedra that ties together its faces, edges, and
-- vertices.
module Rubik.Polyhedron where

import Data.Array.IArray ((!), Array, Ix, array, listArray)
import Data.Bits ((.|.), bit, shiftL)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (inits, tails)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, maybeToList)


-- | A class for the faces of polyhedra used as the bases for Rubik-style
-- puzzles; contains associated types for the edges and verrtices.
class (Enum f, Bounded f, Ix f,
       Enum (PolyEdge f), Bounded (PolyEdge f),
       Enum (PolyVertex f), Bounded (PolyVertex f))
      => PolyFace f where
  data PolyEdge f
  data PolyVertex f

  faceNames :: [(f, Char)]
  -- ^ Every face of the polyhedron must be identified by a single character,
  -- and listed here.

  neighboringFaces :: f -> [f]
  -- ^ The faces that touch a given face, in clockwise order.

  allEdgesAsFaces :: [[f]]
  -- ^ All the edges, as length-2 lists of faces with the distinguished face
  -- first.

  allVerticesAsFaces :: [[f]]
  -- ^ All the vertices, as lists of their faces.  The faces must appear in
  -- clockwise order, starting with the vertex's distinguished face.


-- | A face's single-character name.
faceToName :: (PolyFace f) => f -> Char
faceToName = (nameArray !)
  where nameArray :: (PolyFace f) => Array f Char
        nameArray = array (minBound, maxBound) faceNames

-- | Converts back from a face's name to the face.
nameToFace :: (PolyFace f) => Char -> f
nameToFace = fromJust . nameToMaybeFace

-- | Converts back from a face's name to the face, if it is one.
nameToMaybeFace :: (PolyFace f) => Char -> Maybe f
nameToMaybeFace = flip Map.lookup faceMap
  where faceMap :: (PolyFace f) => Map Char f
        faceMap = Map.fromList $ map (\(f, c) -> (c, f)) faceNames

-- | Converts a list of faces into an int, as a bit set.
facesIndex :: forall f. (PolyFace f) => [f] -> Int
facesIndex [] = 0
facesIndex (f:fs) = toBit f .|. facesIndex fs
  where toBit f = bit $ fromEnum f - fromEnum (minBound::f)

-- | The neighbor just counterclockwise of the given face's given neighbor.
previousNeighbor :: (PolyFace f) => f -> f -> f
previousNeighbor f n = if n == head ns then last ns else prev ns
  where ns = neighboringFaces f
        prev (f:fs@(f2:_)) = if n == f2 then f else prev fs

-- | The neighbor just clockwise of the given face's given neighbor.
nextNeighbor :: (PolyFace f) => f -> f -> f
nextNeighbor f n = next ns
  where ns = neighboringFaces f
        next (f:fs@(f2:_)) = if n == f then f2 else next fs
        next [f] = if n == f then head ns else undefined

-- | The edges of a given face, as length-2 lists of their faces.  The given
-- face appears first.
faceEdgesAsFaces :: (PolyFace f) => f -> [[f]]
faceEdgesAsFaces f = map (\f2 -> [f, f2]) $ neighboringFaces f

-- | The canonical name of an edge.
edgeName :: (PolyFace f) => PolyEdge f -> String
edgeName = map faceToName . edgeFaces

-- | The 2 faces of an edge, in canonical order.
edgeFaces :: (PolyFace f) => PolyEdge f -> [f]
edgeFaces = (facesArray !) . fromEnum
  where facesArray = makeFacesArray allEdgesAsFaces

-- | Converts a string containing 2 face names into the
-- corresponding edge.
nameToEdge :: (PolyFace f) => String -> PolyEdge f
nameToEdge = facesToEdge . map nameToFace

-- | Converts a pair of faces into the corresponding edge.
facesToEdge :: (PolyFace f) => [f] -> PolyEdge f
facesToEdge = fromJust . facesToMaybeEdge

-- | Converts a pair of faces into the corresponding edge, if there is one.
facesToMaybeEdge :: (PolyFace f) => [f] -> Maybe (PolyEdge f)
facesToMaybeEdge = flip IntMap.lookup edgeMap . facesIndex
  where edgeMap = makeFaceBitsetMap edgeFaces

-- | The edges that belong to a given face, in clockwise order.
faceEdges :: (PolyFace f) => f -> [PolyEdge f]
faceEdges = (edgesArray !)
  where edgesArray :: (PolyFace f) => Array f [PolyEdge f]
        edgesArray = listArray (minBound, maxBound)
                     [es f | f <- [minBound..]]
        es :: (PolyFace f) => f -> [PolyEdge f]
        es = map facesToEdge . efs
        efs f = map (\f2 -> [f, f2]) (neighboringFaces f)


-- | The vertices of a given face, as lists of the faces that meet at each
-- vertex.  The faces for each vertex appear in clockwise order starting with
-- the given face.  And the vertices corresponding to the lists of faces are
-- also in clockwise order.
faceVerticesAsFaces :: forall f. (PolyFace f) => f -> [[f]]
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

-- | The canonical name of a vertex.
vertexName :: (PolyFace f) => PolyVertex f -> String
vertexName = map faceToName . vertexFaces

-- | The faces of a vertex, in canonical order.
vertexFaces :: (PolyFace f) => PolyVertex f -> [f]
vertexFaces = (facesArray !) . fromEnum
  where facesArray = makeFacesArray allVerticesAsFaces

-- | Converts a string containing face names into the corresponding vertex.
nameToVertex :: (PolyFace f) => String -> PolyVertex f
nameToVertex = facesToVertex . map nameToFace

-- | Converts a list of faces into the corresponding vertex.
facesToVertex :: (PolyFace f) => [f] -> PolyVertex f
facesToVertex = fromJust . facesToMaybeVertex

-- | Converts a list of faces into the corresponding vertex, if there is one.
facesToMaybeVertex :: (PolyFace f) => [f] -> Maybe (PolyVertex f)
facesToMaybeVertex = flip IntMap.lookup vertexMap . facesIndex
  where vertexMap = makeFaceBitsetMap vertexFaces

-- | The vertices that belong to a given face, in clockwise order.
faceVertices :: forall f. (PolyFace f) => f -> [PolyVertex f]
faceVertices = (verticesArray !)
  where verticesArray :: Array f [PolyVertex f]
        verticesArray = listArray (minBound, maxBound)
                        [vs f | f <- [minBound..]]
        vs :: f -> [PolyVertex f]
        vs = map facesToVertex . faceVerticesAsFaces


-- | A helper to implement edgeFaces and vertexFaces.  Makes an array from a
-- list of lists of faces, such as allVerticesAsFaces and allEdgesAsFaces.
makeFacesArray :: [[f]] -> Array Int [f]
makeFacesArray fss = listArray (0, length fss - 1) fss

-- | A helper to implement facesToMaybeEdge and facesToMaybeVertex.  Makes a map
-- that maps a bit-set of face numbers to either edges or vertices.
makeFaceBitsetMap :: (Enum a, Bounded a, PolyFace f) =>
                     (a -> [f]) -> IntMap a
makeFaceBitsetMap xxFaces =
  IntMap.fromList [(i, a) | a <- [minBound..], let i = facesIndex $ xxFaces a]

-- | A helper to implement the toEnum methods of edge and vertex types.
toBoundedEnum :: forall a. (Bounded a, Enum a) => (Int -> a) -> Int -> a
toBoundedEnum ctor i = if i < fromEnum (minBound::a) || i > fromEnum (maxBound::a)
                       then undefined
                       else ctor i

-- | A helper to implement allVerticesAsFaces for polyhedra with 3 faces per
-- vertex.
faceNeighborTriples :: (PolyFace f) => f -> [[f]]
faceNeighborTriples f = vt ns $ tail $ cycle ns
  where ns = neighboringFaces f
        vt [] _ = []
        vt (x:xs) (y:ys) = [f, x, y] : vt xs ys


-- | A helper to implement Read for face types.
readSFace :: (PolyFace f) => ReadS f
readSFace (c:cs) = maybeToList $ do
  f <- nameToMaybeFace c
  return (f, cs)
readSFace _ = []

-- | A helper to implement Read for edge types.
readSEdge :: (PolyFace f) => ReadS (PolyEdge f)
readSEdge (c1:c2:cs) = maybeToList $ do
  f1 <- nameToMaybeFace c1
  f2 <- nameToMaybeFace c2
  e <- facesToMaybeEdge [f1, f2]
  return (e, cs)
readSEdge _ = []

-- | A helper to implement Read for vertex types (for polyhedra whose vertices
-- have 3 faces).
readSVertex :: (PolyFace f) => ReadS (PolyVertex f)
readSVertex (c1:c2:c3:cs) = maybeToList $ do
  f1 <- nameToMaybeFace c1
  f2 <- nameToMaybeFace c2
  f3 <- nameToMaybeFace c3
  v <- facesToMaybeVertex [f1, f2, f3]
  return (v, cs)
readSVertex _ = []
