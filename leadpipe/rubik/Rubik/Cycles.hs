{-# LANGUAGE ScopedTypeVariables, TypeSynonymInstances, EmptyDataDecls #-}
module Rubik.Cycles where

import Rubik.Algebra
import Rubik.Lists
import Rubik.Strings

import Data.Array ((//), array, elems)
import Data.List (elemIndex)
import qualified Data.Map as Map
import Data.Maybe (fromJust)


data T2
instance IntAsType T2 where value _ = 2

data T3
instance IntAsType T3 where value _ = 3

data T4
instance IntAsType T4 where value _ = 4

data T5
instance IntAsType T5 where value _ = 5

type Z2 = Zn T2
type Z3 = Zn T3
type Z4 = Zn T4
type Z5 = Zn T5

data Tw1
instance IntAsType Tw1 where
  value _ = 1
  showsInt _ _ = id

data Tw2
instance IntAsType Tw2 where
  value _ = 2
  showsInt _ = showTwist
    where showTwist 0 = id
          showTwist 1 = showChar '+'

data Tw3
instance IntAsType Tw3 where
  value _ = 3
  showsInt _ = showTwist
    where showTwist 0 = id
          showTwist 1 = showChar '+'
          showTwist 2 = showChar '-'

data Tw4
instance IntAsType Tw4 where
  value _ = 4
  showsInt _ = showTwist
    where showTwist 0 = id
          showTwist 1 = showChar '+'
          showTwist 2 = showChar '='
          showTwist 3 = showChar '-'

data Tw5
instance IntAsType Tw5 where
  value _ = 5
  showsInt _ = showTwist
    where showTwist 0 = id
          showTwist 1 = showChar '+'
          showTwist 2 = showString "++"
          showTwist 3 = showString "--"
          showTwist 4 = showChar '-'

type Twistless = Zn Tw1
type Flip = Zn Tw2
type Twist3 = Zn Tw3
type Twist4 = Zn Tw4
type Twist5 = Zn Tw5


-- | Tells whether a wreath leaves a (sorted) list of indices alone.
leavesUnmoved (Wreath list) indices = f 0 indices list
    where f _ [] _ = True
          f _ _ [] = True
          f j (i:is) (m:ms)
            | j == i    = m == WM (i, one) && f (j+1) is ms
            | otherwise = f (j+1) (i:is) ms

getIndex (WM (i, _)) = i

toCycles' :: forall t. (Ord t, Group t) => Wreath t -> [[WreathMove t]]
toCycles' (Wreath []) = []
toCycles' (Wreath list) =
    let mappings :: Map.Map Int (WreathMove t)
        mappings = Map.fromDistinctAscList (zip [0..] list)
    in findCycles ([], 0, [], mappings)
    where
      findCycles (cs, i, c, mappings) =
          if Map.null mappings
          then reverse (map invert (c:cs))
          else
              case Map.lookup i mappings of
                Nothing ->
                    let (j, m) = Map.findMin mappings
                    in findCycles (c:cs, getIndex m, [m], Map.delete j mappings)
                Just m -> findCycles (cs, getIndex m, m:c, Map.delete i mappings)
      invert (m:ms) = m:(reverse ms)


-- | Converts a wreath into disjoint cycles.
toCycles :: (Ord t, Group t) => Wreath t -> [[WreathMove t]]
toCycles w = filter (not . isUnmoved) (toCycles' w)
    where isUnmoved [(WM (i,t))] = t == one
          isUnmoved [] = True
          isUnmoved _ = False


-- | Converts a list of cycles into a wreath.
fromCycles :: (Ord t, Group t) => [[WreathMove t]] -> Wreath t
fromCycles cs = Wreath (elems (array (0, n) [(i, WM (i, one)) | i <- [0..n]] //
                               (concatMap fromCycle cs)))
    where n = maximum (map getIndex (concat cs))
          fromCycle ms = zip (map getIndex ms) (rotate 1 ms)


-- | Creates a cycle from a face, a list of pieces, and a function
-- that maps a piece to its constituent faces.  Calculates the twist
-- for each piece as the difference in location of the face within
-- each pair of pieces' faces.
asCycle :: forall f p t. (Eq f, Enum p, Group t, Ord t, Num t) =>
           f -> [p] -> (p -> [f]) -> [WreathMove t]
asCycle f ps toFs = zipWith toWM ps twists
  where toWM p t = WM (fromEnum p, t)
        indices = map indexIn ps
        indexIn p = toInteger $ fromJust $ f `elemIndex` toFs p
        twists = zipWith toTwist indices $ rotate 1 indices
        toTwist i j = fromInteger (i - j)


-- | A variant of asCycle for when the list of pieces is more readily
-- accessible as a function of the face.
asCycle' :: forall f p t. (Eq f, Enum p, Group t, Ord t, Num t) =>
            f -> (f -> [p]) -> (p -> [f]) -> [WreathMove t]
asCycle' f toPs toFs = asCycle f (toPs f) toFs


-- | Creates a cycle from a list of pieces, for situations where no
-- twisting is possible.
asSimpleCycle :: forall p t. (Enum p, Group t, Ord t) => [p] -> [WreathMove t]
asSimpleCycle ps = map toWM ps
  where toWM p = WM (fromEnum p, one)


-- | Optionally shows a wreath as its disjoint cycles, given a way to show
-- moves; an empty wreath returns Nothing.
optShowCycles :: (Ord t, Group t) => (WreathMove t -> ShowS) -> Wreath t -> OptS
optShowCycles showMove w = showCycles' (toCycles w)
  where showCycles' [] = Nothing
        showCycles' [[]] = Nothing
        --showCycles' [[m]] = showMove m
        showCycles' [c] = toOptS $ showParen True (showMoves c)
        showCycles' (c:cs) = showCycles' [c] *> showCycles' cs
        showMoves [m] = showMove m
        showMoves (m:ms) = showMove m . showChar ' ' . showMoves ms

-- | Shows a move, given a way to convert its index to a showable value.
showMove :: (Ord t, Group t, Show a, Show t) =>
            (Int -> a) -> WreathMove t -> ShowS
showMove fromInt (WM (i,t)) = shows (fromInt i) . shows t

-- | Shows a pair of empty parentheses.
showEmptyParens :: ShowS
showEmptyParens = showString "()"

-- | Optionally shows a wreath as its disjoint cycles, given a way to convert a
-- move's index into a showable value.
optShowCyclesDefault :: (Ord t, Group t, Show a, Show t) =>
                        (Int -> a) -> Wreath t -> OptS
optShowCyclesDefault fromInt = optShowCycles (showMove fromInt)

-- | Converts an OptS of cycles into a ShowS.
fromOptCycles :: OptS -> ShowS
fromOptCycles = fromOptS showEmptyParens
