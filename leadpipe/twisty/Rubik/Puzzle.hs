{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, TypeFamilies #-}

-- | Defines types representing Rubik-style puzzles and algorithms for
-- manipulating them.
module Rubik.Puzzle where

import Rubik.Group

import Data.List (foldl')
import Data.Maybe (listToMaybe)
import Data.Monoid (Monoid, mappend, mempty)
import Text.ParserCombinators.ReadP
import Text.ParserCombinators.ReadPrec (lift)
import Text.Read hiding ((<++))


-- | A class for the moves that can be performed on a Rubik-style puzzle.
class (Eq m, Read m, Show m) => PuzzleMove m where
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


-- | A class for Rubik-style puzzle states.
class (Group p, Show p, PuzzleMove (Move p)) => Puzzle p where

  type Move p
  -- ^ The associated move type.  For example, a move for a standard Rubik's
  -- cube might be "twist the top face a quarter-turn clockwise."

  fromMove :: Move p -> p
  -- ^ Converts the given move to the associated puzzle state.


-- | An Algorithm combines a list of moves with the resulting puzzle state.  The
-- list is reversed: new moves go on the head.
data (Puzzle p) => Algorithm p = Algorithm
                                 { moves :: [Move p]
                                 , result :: p
                                 }

-- | Is this a non-trivial algorithm?
isNontrivial :: (Puzzle p) => Algorithm p -> Bool
isNontrivial = not . null . moves

-- | For non-trivial algorithms only, the algorithm's last move.  Because
-- Algorithm canonicalizes the order of moves, this could be different from the
-- move you most recently appended.
lastMove :: (Puzzle p) => Algorithm p -> Move p
lastMove = head . moves

-- | Equality testing for Algorithms.
instance (Puzzle p) => Eq (Algorithm p) where
  a == b = moves a == moves b

-- | Algorithms are groups.
instance (Puzzle p) => Monoid (Algorithm p) where
  mempty = Algorithm [] one
  mappend (Algorithm ms1 s1) (Algorithm ms2 s2) = Algorithm ms (s1 $* s2)
    where ms = foldr prependMove ms2 ms1 -- Note the reversed order

-- | Algorithms are groups.
instance (Puzzle p) => Group (Algorithm p) where
  ginvert (Algorithm ms p) = Algorithm (map undoMove . reverse $ ms) (ginvert p)

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

-- | Algorithms are displayed as their moves in order, some spaces, and the
-- resulting puzzle state.
instance (Puzzle p) => Show (Algorithm p) where
  showsPrec _ (Algorithm ms p) = showMoves ms . showString "    " . shows p
    where showMoves = foldl' op id
          f `op` m = shows m . f

instance (Puzzle p) => Read (Algorithm p) where
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
