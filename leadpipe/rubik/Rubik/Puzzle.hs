{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, TypeFamilies #-}

-- | Defines types representing Rubik-style puzzles and algorithms for
-- manipulating them.
module Rubik.Puzzle where

import Rubik.Algebra

import Data.List (foldl')
import Data.Maybe (listToMaybe)
import Data.Monoid (Monoid, mappend, mempty)
import Text.ParserCombinators.ReadP
import Text.ParserCombinators.ReadPrec (lift)
import Text.Read hiding ((<++))


-- | A class for the moves that can be performed on a Rubik-style puzzle.
class (Read m, Show m) => PuzzleMove m where
  undoMove :: m -> m
  -- ^ The move that undoes the given move.  This means that @joinMoves m
  -- (undoMove m)@ (and vice versa) must equal the empty list.
  --
  -- Also, for any Puzzle instance that uses this move type, @fromMove m *>
  -- fromMove (undoMove m)@ (and vice versa) must equal the Puzzle's @one@.

  joinMoves :: m -> m -> [m]
  -- ^ Combines two moves.  If one is the opposite of the other, returns the
  -- empty list.  If they can be combined into a single move, returns a list
  -- containing that single move.  If they do not interact, returns the same two
  -- moves but in a canonical order (ie, passing the same two moves in the
  -- opposite order will return the same list).  And if they do interact,
  -- returns the same two moves in the same order.


-- | A class for Rubik-style puzzle states.
class (Group s, Show s, PuzzleMove (Move s)) => Puzzle s where

  type Move s
  -- ^ The associated move type.  For example, a move for a standard Rubik's
  -- cube might be "twist the top face a quarter-turn clockwise."

  fromMove :: Move s -> s
  -- ^ Converts the given move to the associated puzzle state.


-- | An Algorithm combines a list of moves with the resulting puzzle state.  The
-- list is reversed: new moves go on the head.
data (Puzzle s) => Algorithm s = Algorithm [Move s] s

-- | Algorithms are groups.
instance (Puzzle s) => Monoid (Algorithm s) where
  mempty = Algorithm [] one
  mappend (Algorithm ms1 s1) (Algorithm ms2 s2) = Algorithm ms (s1 *> s2)
    where ms = foldr prependMove ms2 ms1 -- Note the reversed order

-- | Algorithms are groups.
instance (Puzzle s) => Group (Algorithm s) where
  ginvert (Algorithm ms s) = Algorithm (map undoMove . reverse $ ms) (ginvert s)

-- | Adds a move to an algorithm.
applyMove :: (Puzzle s) => Algorithm s -> Move s -> Algorithm s
applyMove (Algorithm ms s) m = Algorithm (prependMove m ms) (s *> fromMove m)

-- | Prepends a move to a list of moves, maintaining canonicalization as
-- implemented by 'joinMoves'.
prependMove :: (PuzzleMove m) => m -> [m] -> [m]
prependMove m [] = [m]
prependMove m (pm:ms) = joinMoves m pm ++ ms

-- | Algorithms are displayed as their moves in order, a tab, and the resulting
-- puzzle state.
instance (Puzzle s) => Show (Algorithm s) where
  showsPrec _ (Algorithm ms s) = showMoves ms . showChar '\t' . shows s
    where showMoves = foldl' op id
          f `op` m = shows m . f

instance (Puzzle s) => Read (Algorithm s) where
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
