{-
Copyright 2011 Google Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

{-# LANGUAGE TemplateHaskell #-}

import Twisty.FaceTwist
import Twisty.Group
import Twisty.Polyhedron
import Twisty.Tetra
import Twisty.Tetra3
import Twisty.Wreath
import Testing

import Data.Array
import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.TH
import Test.QuickCheck

main = $(defaultMainGenerator)

-- Ensure a face's neighbors don't include the face.
prop_neighborsDiffer :: Face -> Bool
prop_neighborsDiffer f = f `notElem` ns
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

-- Every edge's faces must be neighbors
prop_edgeFacesNeighbors :: Edge -> Bool
prop_edgeFacesNeighbors = allNeighbors . edgeFaces
  where allNeighbors [a, b] = neighbors a b && neighbors b a
        neighbors a b = a `elem` neighboringFaces b

-- For every face's basic move, its resulting state must have edge and vertex
-- twists that add up to 0 ("one" in Group-speak).
prop_faceMoveTotalTwistZero :: Face -> Bool
prop_faceMoveTotalTwistZero f = twistsSumZero $ fromMove1 $ FaceTwist f 1 1
  where twistsSumZero (Tetra3 (v, e)) = sumTwists v == one && sumTwists e == one
        sumTwists w = sum $ map entryTwist $ map snd $ getAlteredEntries w
