{-# LANGUAGE TypeFamilies #-}
-- | Defines the 3x3 tetrahedron puzzle.
module Twisty.Tetra3 where

import Twisty.Cycles
import Twisty.FaceTwist
import Twisty.Group
import qualified Twisty.Memo as Memo
import Twisty.Polyhedron
import Twisty.Puzzle
import Twisty.Tetra
import Twisty.Twists
import Twisty.Wreath

import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Data.Monoid (Monoid, mappend, mempty)

newtype Tetra3 = Tetra3 (Wreath Vertex, Wreath Edge) deriving (Eq, Ord)

instance Monoid Tetra3 where
  mempty = Tetra3 one
  mappend (Tetra3 s1) (Tetra3 s2) = Tetra3 (s1 $* s2)

instance Group Tetra3 where
  ginvert (Tetra3 s) = Tetra3 (ginvert s)

instance Puzzle Tetra3 where
  type Move Tetra3 = TetraMove1
  fromMove = Memo.array fromMove1

fromMove1 :: TetraMove1 -> Tetra3
fromMove1 (FaceTwist f _ 1) = Tetra3 (v, e)
  where v = fromCycles [asCycle' f faceVertices vertexFaces]
        e = fromCycles [asCycle' f faceEdges edgeFaces]
fromMove1 (FaceTwist f d n) = fromMove (FaceTwist f d 1) $^ n

instance Show Tetra3 where
  showsPrec _ (Tetra3 (v, e)) = fromOptCycles $ optShowCycles v $* optShowCycles e

t3 :: String -> Algorithm Tetra3
t3 = read
