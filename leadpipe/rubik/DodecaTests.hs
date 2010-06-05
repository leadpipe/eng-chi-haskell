{-# OPTIONS_GHC -fglasgow-exts -XTemplateHaskell #-}

import Dodeca
import Rubik
import Testing

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
prop_fiveNeighbors f = length (neighboringFaces f) == 5

-- Every vertex's faces must be in clockwise order.
prop_vertexFacesClockwise v = allClockwise (vertexFaces v)
  where allClockwise (a, b, c) = cw a b c && cw b c a && cw c a b
        cw a b c = adjacent b c $ cycle (neighboringFaces a)
        adjacent b c (x:y:xs)
          | x == b    = y == c
          | otherwise = adjacent b c (y:xs)
