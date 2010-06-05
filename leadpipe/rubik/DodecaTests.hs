{-# OPTIONS_GHC -fglasgow-exts -XTemplateHaskell #-}

import Rubik.Dodeca
import Rubik.Geometry
import Testing

import Data.List (elemIndex)
import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.TH
import Test.QuickCheck

main = $(defaultMainGenerator)

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
-- the second face.
prop_vertexDistinguishedFace = check . vertexFaces
  where check [a, b, c] = a `elem` poles ||
                          (b `notElem` poles && c `notElem` poles &&
                           a `oppositeHemisphere` b)

poles = [An, As]
oppositeHemisphere a b = isNorth a /= isNorth b
isNorth f = f <= Fn

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
            where pole = if isNorth a then An else As
                  pns  = neighboringFaces pole
                  rpns = rotate 1 pns
                  opns = map oppositeFace $ rotate 3 pns
