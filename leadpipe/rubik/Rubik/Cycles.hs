{-# LANGUAGE ScopedTypeVariables, TypeSynonymInstances, EmptyDataDecls #-}
module Rubik.Cycles where

import Rubik.Algebra

import Data.Array ((//), array, elems)
import qualified Data.Map as Map
import Data.Monoid (Monoid, mappend, mempty)


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

data EF
instance IntAsType EF where
  value _ = 2
  showsInt _ = showEdgeFlip

showEdgeFlip 0 = id
showEdgeFlip 1 = showChar '+'

type EdgeFlip = Zn EF

data VT
instance IntAsType VT where
  value _ = 3
  showsInt _ = showVertexTwist

showVertexTwist 0 = id
showVertexTwist 1 = showChar '+'
showVertexTwist 2 = showChar '-'

type VertexTwist = Zn VT


--type NoFaceTwist = ()
--type TriangleFaceTwist = Z3
--type SquareFaceTwist = Z4
--type PentagonFaceTwist = Z5


-- Specific wreath types
type EdgeWreath = Wreath EdgeFlip
type VertexWreath = Wreath VertexTwist


-- | Tells whether a wreath leaves a (sorted) list of indices alone.
leavesUnmoved (Wreath list) indices = f 0 indices list
    where f _ [] _ = True
          f _ _ [] = True
          f j (i:is) (m:ms)
            | j == i    = m == WM i one && f (j+1) is ms
            | otherwise = f (j+1) (i:is) ms

getIndex (WM i _) = i

toCycles' :: forall t. (Ord t, Monoid t) => Wreath t -> [[WreathMove t]]
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
toCycles :: (Ord t, Monoid t) => Wreath t -> [[WreathMove t]]
toCycles w = filter (not . isUnmoved) (toCycles' w)
    where isUnmoved [(WM i t)] = t == one
          isUnmoved [] = True
          isUnmoved _ = False


-- | Converts a list of cycles into a wreath.
fromCycles :: (Ord t, Monoid t) => [[WreathMove t]] -> Wreath t
fromCycles cs = Wreath (elems (array (0, n) [(i, WM i one) | i <- [0..n]] //
                               (concatMap fromCycle cs)))
    where n = maximum (map getIndex (concat cs))
          fromCycle ms = zip (map getIndex ms) (rotate ms)
          rotate (x:xs) = xs ++ [x]


-- | Shows a wreath as its disjoint cycles, given a way to show moves.
showCycles :: (Ord t, Monoid t) =>
              (WreathMove t -> String -> String) -> Wreath t -> String -> String
showCycles showMove w = showCycles' (toCycles w)
  where showCycles' [] = showString "()"
        showCycles' [[]] = showCycles' []
        showCycles' [[m]] = showMove m
        showCycles' [c] = showParen True (showMoves c)
        showCycles' (c:cs) = showCycles' [c] . showCycles' cs
        showMoves [m] = showMove m
        showMoves (m:ms) = showMove m . showChar ' ' . showMoves ms