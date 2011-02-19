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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Defines types representing twisty puzzles and algorithms for manipulating
-- them.
module Twisty.Puzzle
       ( Puzzle(..)
       , PuzzleMove(..)
       , Algorithm
       , result
       , moves
       , moveCount
       , isNontrivial
       , lastMove
       , foldMoves
       , applyMove
       , morph
       )
where

import Twisty.Group

import Data.List (foldl')
import Data.Maybe (listToMaybe)
import Data.Monoid (Monoid, mappend, mempty)
import Text.ParserCombinators.ReadP
import Text.ParserCombinators.ReadPrec (lift)
import Text.Read hiding ((<++))


-- | A class for the moves that can be performed on a twisty puzzle.
class Eq m => PuzzleMove m where
  undoMove :: m -> m
  -- ^ The move that undoes the given move.  This means that @joinMoves m
  -- (undoMove m)@ (and vice versa) must equal the empty list.
  --
  -- Also, for any Puzzle instance that uses this move type, @fromMove m $*
  -- fromMove (undoMove m)@ (and vice versa) must equal the Puzzle's @one@.

  joinMoves :: m -> m -> [m]
  -- ^ Combines two moves.  If they are opposites, returns the empty list.  If
  -- they can be combined into a single move, returns a list containing that
  -- single move.  If they do not interact, returns the same two moves but in a
  -- canonical order (ie, passing the same two moves in the opposite order will
  -- return the same list).  And if they do interact, returns the same two moves
  -- in the same order.

  isTrivialMove :: m -> Bool
  -- ^ Tells whether the given move is trivial, ie, is equivalent to not moving.


-- | A class for twisty puzzle states.
class (Group p, Show p, PuzzleMove (Move p)) => Puzzle p where

  type Move p
  -- ^ The associated move type.  For example, a move for a standard Rubik's
  -- cube might be \"twist the top face a quarter-turn clockwise.\"

  fromMove :: Move p -> p
  -- ^ Converts the given move to the associated puzzle state.


-- | An Algorithm combines a list of moves with the resulting puzzle state.
data (Puzzle p) => Algorithm p =
  Algorithm
  { rMoves :: [Move p] -- ^ The list of moves, in reverse order (most recent first).
  , result :: p        -- ^ The resulting puzzle state.
  }

-- | Is this a non-trivial algorithm?
isNontrivial :: (Puzzle p) => Algorithm p -> Bool
isNontrivial = not . null . rMoves

-- | For non-trivial algorithms only, the algorithm's last move.  Because
-- Algorithm canonicalizes the order of moves, this could be different from the
-- move you most recently appended.
lastMove :: (Puzzle p) => Algorithm p -> Move p
lastMove = head . rMoves

-- | The list of moves that comprises the algorithm.
moves :: (Puzzle p) => Algorithm p -> [Move p]
moves = reverse . rMoves

-- | The number of moves in an algorithm.
moveCount :: (Puzzle p) => Algorithm p -> Int
moveCount = length . rMoves

-- | Accumulates values over an algorithm's moves, from most recent to oldest
-- move.
foldMoves :: (Puzzle p, Monoid m) => (Move p -> m) -> Algorithm p -> m
foldMoves op = foldl' (\val -> (val $*) . op) one . rMoves

-- | Equality testing for Algorithms.  Just comparing the moves works because we
-- canonicalize their order.
instance (Puzzle p) => Eq (Algorithm p) where
  a == b = rMoves a == rMoves b

-- | Algorithms are groups.
instance (Puzzle p) => Monoid (Algorithm p) where
  mempty = Algorithm [] one
  mappend (Algorithm ms1 s1) (Algorithm ms2 s2) = Algorithm ms (s1 $* s2)
    where ms = foldr prependMove ms1 ms2

-- | Algorithms are groups.
instance (Puzzle p) => Group (Algorithm p) where
  ginvert (Algorithm ms p) = Algorithm ms' (ginvert p)
    where ms' = foldl' (flip (prependMove . undoMove)) [] ms

-- | Adds a move to an algorithm.
applyMove :: (Puzzle p) => Algorithm p -> Move p -> Algorithm p
applyMove a@(Algorithm ms p) m = Algorithm (prependMove m ms) (p $* fromMove m)

-- | Prepends a move to a list of moves, maintaining canonicalization as
-- implemented by 'joinMoves'.
prependMove :: (PuzzleMove m) => m -> [m] -> [m]
prependMove m []
  | isTrivialMove m = []
  | otherwise       = [m]
prependMove m l@(pm:ms)
  | isTrivialMove m = l
  | otherwise       = case joinMoves m pm of
    [] -> ms
    [m'] -> m' : ms
    [m1, m2] -> if m2 == pm then m1 : l
                else m1 : prependMove m2 ms

-- | Transforms an algorithm to a related algorithm by transforming each move in
-- the list.
morph :: forall p. (Puzzle p) => (Move p -> Move p) -> Algorithm p -> Algorithm p
morph f (Algorithm ms _) = foldr g one ms
  where g = flip applyMove . f

-- | Algorithms are displayed as their moves in order, some spaces, and the
-- resulting puzzle state.
instance (Puzzle p, Show (Move p)) => Show (Algorithm p) where
  showsPrec _ (Algorithm ms p) = showMoves ms . showString "    " . shows p
    where showMoves = foldl' op id
          f `op` m = shows m . f

instance (Puzzle p, Read (Move p)) => Read (Algorithm p) where
  readPrec = lift $ readAndApply mempty
    where readAndApply alg =
            do skipSpaces
               m <- readS_to_P reads
               readAndApply (alg `applyMove` m)
            <++
            do optional skipParens
               return alg

          skipParens = do
            skipSpaces
            char '('
            munch (\c -> c /= '(' && c /= ')')
            optional skipParens
            char ')'
            optional skipParens

  readList = readListDefault
  readListPrec = readListPrecDefault
