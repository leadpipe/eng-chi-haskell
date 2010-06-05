{-# OPTIONS_GHC -fglasgow-exts -XTemplateHaskell #-}

import Cube
import Rubik
import Testing

import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.TH
import Test.QuickCheck

main = $(defaultMainGenerator)

-- Ensure a face is not its own opposite.
prop_oppositesDiffer f = f /= oppositeFace f

-- Ensure a face's neighbors don't include either the face or its
-- opposite.
prop_neighborsDiffer f = notElem f ns && notElem (oppositeFace f) ns
  where ns = neighboringFaces f

-- Ensure a face's neighbors list doesn't have opposites next to each
-- other.
prop_adjacentNeighborsNotOpposite f = not . or $ zipWith isOpposite ns rns
  where ns = neighboringFaces f
        rns = tail ns ++ [head ns] -- rotated neighbors

-- Every face must have exactly 4 neighbors.
prop_fourNeighbors f = length (neighboringFaces f) == 4

-- Every vertex's faces must be neighbors and in clockwise order.
prop_vertexFacesClockwise = allClockwise . vertexFaces
  where allClockwise [a, b, c] = cw a b c && cw b c a && cw c a b
        cw a b c = adjacent b c $ cycle (neighboringFaces a)
        adjacent b c (x:y:xs)
          | x == b    = y == c
          | otherwise = adjacent b c (y:xs)

-- Every vertex's distinguished face must be Up or Down.
prop_vertexDistinguishedFace v = distinguishedFace v `elem` [U, D]
  where distinguishedFace = head . vertexFaces

-- Every edge's faces must be neighbors
prop_edgeFacesNeighbors = allNeighbors . edgeFaces
  where allNeighbors [a, b] = neighbors a b && neighbors b a
        neighbors a b = a `elem` neighboringFaces b

-- Every edge's distinguished face must be Up or Down if possible, or
-- Front or Back otherwise.
prop_edgeDistinguishedFace = check . edgeFaces
  where check [a, b] = a `elem` [U, D] ||
                       (a `elem` [F, B] && b `notElem` [U, D])
