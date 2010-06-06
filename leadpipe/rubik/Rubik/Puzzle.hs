{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses, ScopedTypeVariables #-}

-- | Defines types representing Rubik-style puzzles, their states, and
-- individual and compound moves that advance them from one state to
-- another.
module Rubik.Puzzle where

import Rubik.Algebra
import Rubik.Geometry

import Data.Bits (shiftR, testBit)
import Data.Monoid (Monoid, mappend, mconcat, mempty)

-- | A single move of a Rubik-style puzzle: twists layers of a face.
data (Eq f, Ord f) => Move f =
  Move { getFace :: !f     -- ^ The face to twist.
       , getLayers :: !Int -- ^ A bit set of the layers to twist.
       , getTwist :: !Int  -- ^ The number of clockwise increments to twist.
       }
  deriving (Eq, Ord)

-- | The Puzzle class ties together a state type with the face, edge,
-- and vertex types of a polyhedron.
class (Monoid s, Eq f, Ord f)
      => Puzzle s f | s -> f where

  -- | Constructs a puzzle state from a single clockwise twist of one
  -- layer of one face from the solved position.
  fromFaceTwist :: f -> Int -> s

  -- | Constructs a puzzle state one move away from the solved
  -- position.
  fromMove :: Move f -> s
  fromMove m = mconcat layerMoves ^> getTwist m
    where layerMoves = [fromFaceTwist (getFace m) layer |
                        layer <- layers 0 (getLayers m)]
          layers i n
            | n <= 0    = []
            | otherwise = let rest = layers (i+1) (n`shiftR`1)
                          in if n`testBit`0 then i : rest else rest

-- | An Algorithm combines a list of moves with the resulting puzzle
-- state.  The list is reversed: new moves go on the head.
data (Puzzle s f) => Algorithm s f = Algorithm [Move f] s

-- | Algorithms are monoids.
instance (Puzzle s f) => Monoid (Algorithm s f) where
  mempty = Algorithm [] one
  mappend (Algorithm m1 s1) (Algorithm m2 s2) =
    Algorithm (m2 *> m1) (s1 *> s2) -- Note the list is backward

-- | Adds a move to an algorithm.
applyMove :: (Puzzle s f) => Algorithm s f -> Move f -> Algorithm s f
applyMove (Algorithm ms s) m = Algorithm (m:ms) (s *> fromMove m)
