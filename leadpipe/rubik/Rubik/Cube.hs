{-# LANGUAGE MultiParamTypeClasses, ScopedTypeVariables, TypeSynonymInstances #-}
module Rubik.Cube where

import Rubik.Geometry

import Data.Array.IArray (Ix, (!), Array, listArray)
import Data.Char (toLower)
import Data.Maybe (maybeToList)
import GHC.Enum (boundedEnumFrom, boundedEnumFromThen)

-- | The faces of the cube.  The order is such that the opposite face
-- for face X is the same distance from the back of the list as X is
-- from the front.  (The names stand for up, front, left, right, back,
-- and down.)
data Face = U | F | L | R | B | D
          deriving (Eq, Ord, Enum, Bounded, Ix)

-- | The edges of the cube.
newtype Edge = Edge Int deriving (Eq, Ord, Ix)

-- | The vertices of the cube.
newtype Vertex = Vertex Int deriving (Eq, Ord, Ix)


oppositeFaceNumber :: Int -> Int
oppositeFaceNumber = (5 -)

oppositeFace :: Face -> Face
oppositeFace = toEnum . oppositeFaceNumber . fromEnum

isOpposite f1 f2 = f1 == oppositeFace f2

allVerticesAsFaces' = faceVertexTriples U ++ faceVertexTriples D

allEdgesAsFaces' = topEdges ++ frontEdges ++ backEdges ++ bottomEdges
  where topEdges = map (\f -> [U, f]) $ neighboringFaces U
        frontEdges = [[F, L], [F, R]]
        backEdges = invert frontEdges
        bottomEdges = invert topEdges
        invert = reverse . map (map oppositeFace)


instance Polyhedron Face Edge Vertex where

  faceNames = [(U, 'u'), (F, 'f'), (L, 'l'),
               (R, 'r'), (B, 'b'), (D, 'd')]

  neighboringFaces = (neighbors !)
    where neighbors :: Array Face [Face]
          neighbors = listArray (minBound, maxBound) [nf i | i <- [0..5]]
          nf n
            | n < 3 = let firstTwo = map toEnum [(n+1)`mod`3, (n+2)`mod`3]
                      in firstTwo ++ map oppositeFace firstTwo
            | otherwise = reverse . nf . oppositeFaceNumber $ n

  allVerticesAsFaces = allVerticesAsFaces'

  allEdgesAsFaces = allEdgesAsFaces'


instance Show Face where
  showsPrec _ = showChar . faceToName

instance Read Face where
  readsPrec _ (c:cs) = maybeToList $ do
    f <- nameToMaybeFace (toLower c)
    return (f, cs)
  readsPrec _ _ = []

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
  readsPrec _ (c1:c2:cs) = maybeToList $ do
    f1 <- nameToMaybeFace (toLower c1)
    f2 <- nameToMaybeFace (toLower c2)
    e <- facesToMaybeEdge [f1, f2]
    return (e, cs)
  readsPrec _ _ = []

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
  readsPrec _ (c1:c2:c3:cs) = maybeToList $ do
    f1 <- nameToMaybeFace (toLower c1)
    f2 <- nameToMaybeFace (toLower c2)
    f3 <- nameToMaybeFace (toLower c3)
    v <- facesToMaybeVertex [f1, f2, f3]
    return (v, cs)
  readsPrec _ _ = []
