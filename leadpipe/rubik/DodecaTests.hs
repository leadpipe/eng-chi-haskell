{-# OPTIONS_GHC -fglasgow-exts -XTemplateHaskell #-}

import Rubik.Dodeca
import Rubik.Lists
import Rubik.Polyhedron
import Testing

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (elemIndex)
import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.TH
import Test.QuickCheck

main = $(defaultMainGenerator)

poles = [An, As]
oppositeHemisphere a b = isNorth a /= isNorth b
isNorth f = f <= Fn


-- Ensure a face is not its own opposite.
prop_oppositesDiffer f = f /= oppositeFace f

-- Ensure a face's neighbors don't include the face, its opposite, or
-- its opposite's neighbors.
prop_neighborsDiffer f = and $ map (`notElem` neighboringFaces f) faces
  where faces = f : oppositeFace f : neighboringFaces (oppositeFace f)

-- Ensure a face's neighbors list doesn't have opposites next to each
-- other.
prop_adjacentNeighborsNotOpposite f = not . or $ zipWith isOpposite ns rns
  where ns = neighboringFaces f
        rns = tail ns ++ [head ns] -- rotated neighbors

-- Every face must have exactly 5 neighbors.
prop_fiveNeighbors :: Face -> Bool
prop_fiveNeighbors f = length (neighboringFaces f) == 5

-- The number of edges must equal the number of unique pairs of faces
-- that determine edges.
prop_edgeFacesUnique = IntMap.size edgeBitsetMap == length edgeList
  where edgeBitsetMap :: IntMap Edge
        edgeBitsetMap = makeFaceBitsetMap edgeFaces
        edgeList :: [[Face]]
        edgeList = allEdgesAsFaces

-- Every edge's faces must be neighbors
prop_edgeFacesNeighbors :: Edge -> Bool
prop_edgeFacesNeighbors = allNeighbors . edgeFaces
  where allNeighbors [a, b] = neighbors a b && neighbors b a
        neighbors a b = a `elem` neighboringFaces b

-- Check an edge's distinguished face against the rules.
prop_edgeDistinguishedFace = check . edgeFaces
  where check [a, b]
          | a `elem` poles           = True
          | a `oppositeHemisphere` b = a `elemIndex` pns == b `elemIndex` opns
          | otherwise                = a `elemIndex` pns == b `elemIndex` rpns
            where nns  = neighboringFaces An
                  pns  = if isNorth a then nns else map oppositeFace nns
                  rpns = rotate 1 pns
                  opns = map oppositeFace $ rotate 3 pns

-- The number of vertices must equal the number of unique sets of faces
-- that determine vertices.
prop_vertexFacesUnique = IntMap.size vertexBitsetMap == length vertexList
  where vertexBitsetMap :: IntMap Vertex
        vertexBitsetMap = makeFaceBitsetMap vertexFaces
        vertexList :: [[Face]]
        vertexList = allVerticesAsFaces

-- Every vertex's faces must be in clockwise order.
prop_vertexFacesClockwise :: Vertex -> Bool
prop_vertexFacesClockwise = allClockwise . vertexFaces
  where allClockwise [a, b, c] = cw a b c && cw b c a && cw c a b
        cw a b c = adjacent b c $ cycle (neighboringFaces a)
        adjacent b c (x:y:xs)
          | x == b    = y == c
          | otherwise = adjacent b c (y:xs)

-- Every vertex's distinguished face must be An or As, if they appear
-- in the vertex's face list, or must be in the opposite hemisphere of
-- the second (north) or third (south) face.
prop_vertexDistinguishedFace = check . vertexFaces
  where check [a, b, c] = a `elem` poles ||
                          (b `notElem` poles && c `notElem` poles &&
                           a `oppositeHemisphere` (if isNorth a then b else c))
