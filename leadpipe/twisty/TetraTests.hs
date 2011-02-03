{-# LANGUAGE TemplateHaskell #-}

import Twisty.Polyhedron
import Twisty.Tetra
import Testing

import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.TH
import Test.QuickCheck

main = $(defaultMainGenerator)

-- Ensure a face's neighbors don't include the face.
prop_neighborsDiffer :: Face -> Bool
prop_neighborsDiffer f = notElem f ns
  where ns = neighboringFaces f

-- Every face must have exactly 3 neighbors.
prop_nNeighbors :: Face -> Bool
prop_nNeighbors f = length (neighboringFaces f) == 3

-- Every vertex's faces must be neighbors and in clockwise order.
prop_vertexFacesClockwise :: Vertex -> Bool
prop_vertexFacesClockwise = allClockwise . vertexFaces
  where allClockwise [a, b, c] = cw a b c && cw b c a && cw c a b
        cw a b c = adjacent b c $ cycle (neighboringFaces a)
        adjacent b c (x:y:xs)
          | x == b    = y == c
          | otherwise = adjacent b c (y:xs)

-- Every vertex's distinguished face must be Front or Down.
prop_vertexDistinguishedFace v = distinguishedFace v `elem` [F, D]
  where distinguishedFace = head . vertexFaces

-- Every edge's faces must be neighbors
prop_edgeFacesNeighbors :: Edge -> Bool
prop_edgeFacesNeighbors = allNeighbors . edgeFaces
  where allNeighbors [a, b] = neighbors a b && neighbors b a
        neighbors a b = a `elem` neighboringFaces b
