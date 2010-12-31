{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses, ScopedTypeVariables #-}

-- | Defines types representing Rubik-style puzzles, their states, and
-- individual and compound moves that advance them from one state to
-- another.
module Rubik.Puzzle where

import Rubik.Algebra
import Rubik.Geometry

import Data.Bits (shiftR, testBit)
import Data.Monoid (Monoid, mappend, mconcat, mempty)

-- | The Puzzle class ties together a state type with the face type of
-- a polyhedron.
class (Group s, Eq f, Ord f, Show f, Read f) => Puzzle s f | s -> f where

  -- | How many layers there are for a given face.
  numLayers :: s -> f -> Int

  -- | How many twists of a given face return to unity.
  numTwists :: s -> f -> Int

  -- | Constructs a puzzle state from a single clockwise twist of one
  -- layer of one face from the solved position.
  fromFaceTwist :: f -> Int -> s


-- | A single move of a Rubik-style puzzle: twists layers of a face.
data (Puzzle s f) => Move s f =
  Move { getFace :: !f     -- ^ The face to twist.
       , getLayers :: !Int -- ^ A bit set of the layers to twist.
       , getTwist :: !Int  -- ^ The number of clockwise increments to twist.
       }
  deriving (Eq, Ord)

instance (Puzzle s f) => Show (Move s f) where
  showsPrec _ m = shows (getFace m) . shows (getLayers m) . shows (getTwist m)

-- | Constructs a puzzle state one move away from the solved position.
fromMove :: (Puzzle s f) => Move s f -> s
fromMove m = mconcat layerMoves ^> getTwist m
  where layerMoves = [fromFaceTwist (getFace m) layer |
                      layer <- layers 0 (getLayers m)]
        layers i n
          | n <= 0    = []
          | otherwise = let rest = layers (i+1) (n`shiftR`1)
                        in if n`testBit`0 then i : rest else rest

-- | An Algorithm combines a list of moves with the resulting puzzle
-- state.  The list is reversed: new moves go on the head.
data (Puzzle s f) => Algorithm s f = Algorithm [Move s f] s

-- | Algorithms are monoids.
instance (Puzzle s f) => Monoid (Algorithm s f) where
  mempty = Algorithm [] one
  mappend (Algorithm m1 s1) (Algorithm m2 s2) =
    Algorithm (m2 *> m1) (s1 *> s2) -- Note the list is backward

-- | Adds a move to an algorithm.
applyMove :: (Puzzle s f) => Algorithm s f -> Move s f -> Algorithm s f
applyMove (Algorithm ms s) m = Algorithm (m:ms) (s *> fromMove m)
