{-# LANGUAGE ScopedTypeVariables, TypeSynonymInstances #-}
module Rubik where

import Data.Array (Array, elems, array, (//), (!))
import Data.Bits
import Data.Char
import qualified Data.Map as Map
import Data.List
import Data.Maybe
import Numeric (showHex)


class (Eq t, Ord t) => Transform t where
    identity :: t
    (*>) :: t -> t -> t
    (^>) :: t -> Int -> t
    t ^> i
        | i == 0 = identity
        | i == 1 = t
        | otherwise = t *> (t ^> (i-1))

instance Transform Int where
    identity = 1
    (*>) = (Prelude.*)

data (Transform t) => Move t = M { index :: Int, transform :: t }
                               deriving (Eq, Ord)

instance (Transform t, Show t) => Show (Move t) where
    show (M i t) = show i ++ "~" ++ show t

-- My take on the polyonimo permutation stuff:
--   http://www.polyomino.f2s.com/david/haskell/hs/PermutationGroups.hs.txt
-- Use 0-based indexes; add transformations
newtype (Transform t) => Permutation t = P [Move t]

permList (P list) = list

-- Look up the move a permutation applies to an index.
i .^ P ms = ms `lookup` i
    where lookup (m:ms) 0 = m
          lookup (m:ms) j = lookup ms (j-1)
          lookup [] _ = M i identity -- If the index isn't there, it's not moved

-- Chain moves through a permutation.
(M i t) *^ p = let (M i' t') = i .^ p in M i' (t *> t')

instance (Transform t) => Transform (Permutation t) where
    identity = P []
    p@(P ms) *> q@(P ns) = P (map (*^ q) ms')
        where ms' = ms ++ [M i identity | i <- [length ms..length ns - 1]]

instance (Transform t) => Eq (Permutation t) where
    P ms == P ns = eqp 0 ms ns
        where eqp i (m:ms) (n:ns) = m == n && eqp (i+1) ms ns
              eqp _ [] [] = True
              eqp i (m:ms) [] = m == M i identity && eqp (i+1) ms []
              eqp i [] (n:ns) = n == M i identity && eqp (i+1) [] ns

instance (Transform t) => Ord (Permutation t) where
    compare p@(P ms) q@(P ns) = if p == q then EQ else compare ms ns

-- Tells whether a permutation leaves a (sorted) list of indices alone.
leavesUnmoved (P list) indices = f 0 indices list
    where f _ [] _ = True
          f _ _ [] = True
          f j (i:is) (m:ms)
            | j == i    = m == M i identity && f (j+1) is ms
            | otherwise = f (j+1) (i:is) ms

toCycles' :: forall t. (Transform t) => Permutation t -> [[Move t]]
toCycles' (P []) = []
toCycles' (P list) =
    let mappings :: Map.Map Int (Move t)
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
                    in findCycles (c:cs, index m, [m], Map.delete j mappings)
                Just m -> findCycles (cs, index m, m:c, Map.delete i mappings)
      invert (m:ms) = m:(reverse ms)

toCycles p = filter (not . isUnmoved) (toCycles' p)
    where isUnmoved [(M i t)] = t == identity
          isUnmoved [] = True
          isUnmoved _ = False

toIndexCycles p = map (map index) (toCycles p)

fromCycles cs = P (elems (array (0,n) [(i,M i identity) | i <- [0..n]] //
                                (concatMap fromCycle cs)))
    where n = maximum (map index (concat cs))
          fromCycle ms = zip (map index ms) (rotate ms)
          rotate (x:xs) = xs ++ [x]

fromIndexCycles cs = fromCycles (map (map (flip M identity)) cs)
fromCycle t c = fromCycles [map (flip M t) c]
fromIndexCycle = fromCycle identity


showCycles showMove p = showCycles' (toCycles p)
    where showCycles' [] = showString "()"
          showCycles' [[]] = showCycles' []
          showCycles' [[m]] = showMove m
          showCycles' [c] = showParen True (showMoves c)
          showCycles' (c:cs) = showCycles' [c] . showCycles' cs
          showMoves [m] = showMove m
          showMoves (m:ms) = showMove m . showChar ' ' . showMoves ms

showIntTransform t = if t < 0 then showChar '-' else id
showIntMove (M i t) = showsPrec 0 i . showIntTransform t

type SimplePermutation = Permutation Int
instance Show SimplePermutation where
    showsPrec _ = showCycles showIntMove

dimensionPermutations :: [(String, SimplePermutation)]
dimensionPermutations = [("", identity),
                         ("i", fromIndexCycle [1,2]),
                         ("j", fromIndexCycle [2,0]),
                         ("k", fromIndexCycle [0,1]),
                         ("s", fromIndexCycle [0,1,2]),
                         ("t", fromIndexCycle [2,1,0])]

dpByName = Map.fromDistinctAscList dimensionPermutations
dpByPerm = Map.fromList $ map swap dimensionPermutations
    where swap (a,b) = (b,a)

faceNames = "LRFBUD" -- in order
faceNumber name = fromJust $ elemIndex name faceNames
oppositeFace :: Int -> Int
oppositeFace = xor 1
isOpposite f1 f2 = f1 == oppositeFace f2
faceParts = (`divMod` 2)

-- Returns the vertices for the given face number, in clockwise order.
vertices face = let (dim, side) = faceParts face
                    l1 = (dim+side+1) `mod` 3
                    l2 = (l1+side+1) `mod` 3
                    (b1, b2) = (bit l1, bit l2)
                    v = shiftL side dim
                in  [v, v+b2, v+b1+b2, v+b1]

-- 0,0: 000 100 110 010
-- 0,1: 001 011 111 101
-- 1,0: 000 001 101 100
-- 1,1: 010 110 111 011
-- 2,0: 000 010 011 001
-- 2,1: 100 101 111 110

