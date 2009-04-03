{-# LANGUAGE ScopedTypeVariables, TypeSynonymInstances #-}
module Rubik where

import Data.Array (elems, array, (//), (!))
import Data.Bits
import qualified Data.Map as Map
import Data.List
import Data.Maybe
import Numeric (showHex)
import System.Random


class (Eq t, Ord t) => Transform t where
    identity :: t
    (*>) :: t -> t -> t

instance Transform Int where
    identity = 1
    (*>) = (Prelude.*)

data (Transform t) => Move t = M { transform :: t, index :: Int }
                               deriving (Eq, Ord)

instance (Transform t, Show t) => Show (Move t) where
    show (M t i) = show t ++ "~" ++ show i

-- My take on the polyonimo permutation stuff:
--   http://www.polyomino.f2s.com/david/haskell/hs/PermutationGroups.hs.txt
-- Use 0-based indexes; add transformations
newtype (Transform t) => Permutation t = P [Move t]

permList (P list) = list

-- Look up the move a permutation applies to an index.
i .^ P ms = ms `lookup` i
    where lookup (m:ms) 0 = m
          lookup (m:ms) j = lookup ms (j-1)
          lookup [] _ = M identity i -- If the index isn't there, it's not moved

-- Chain moves through a permutation.
(M t i) *^ p = let (M t' i') = i .^ p in M (t *> t') i'

instance (Transform t) => Transform (Permutation t) where
    identity = P []
    p@(P ms) *> q@(P ns) = P (map (*^ q) ms')
        where ms' = ms ++ [M identity i | i <- [length ms..length ns - 1]]

instance (Transform t) => Eq (Permutation t) where
    P ms == P ns = eqp 0 ms ns
        where eqp i (m:ms) (n:ns) = m == n && eqp (i+1) ms ns
              eqp _ [] [] = True
              eqp i (m:ms) [] = m == M identity i && eqp (i+1) ms []
              eqp i [] (n:ns) = n == M identity i && eqp (i+1) [] ns

instance (Transform t) => Ord (Permutation t) where
    compare p@(P ms) q@(P ns) = if p == q then EQ else compare ms ns

-- Tells whether a permutation leaves a (sorted) list of indices alone.
leavesUnmoved (P list) indices = f 0 indices list
    where f _ [] _ = True
          f _ _ [] = True
          f j (i:is) (m:ms)
            | j == i    = m == M identity i && f (j+1) is ms
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
    where isUnmoved [(M t i)] = t == identity
          isUnmoved [] = True
          isUnmoved _ = False

toIndexCycles p = map (map index) (toCycles p)

fromCycles cs = P (elems (array (0,n) [(i,M identity i) | i <- [0..n]] //
                                (concatMap fromCycle cs)))
    where n = maximum (map index (concat cs))
          fromCycle ms = zip (map index ms) (rotate ms)
          rotate (x:xs) = xs ++ [x]

fromIndexCycles cs = fromCycles (map (map (M identity)) cs)
fromCycle t c = fromCycles [map (M t) c]
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
showIntMove (M t i) = showIntTransform t . showsPrec 0 i

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

showVectorMove (M t i) = showString ((Map.!) dpByPerm t) . showsPrec 0 i
showEdgeMove (M t i) = showIntTransform t . showHex i

data RubikPermutation = R { v :: Permutation SimplePermutation,
                            e :: SimplePermutation }
                        deriving (Eq, Ord)

instance Show RubikPermutation where
    showsPrec _ (R v e) = showCycles showVectorMove v .
                          showString " | " .
                          showCycles showEdgeMove e

instance Transform RubikPermutation where
    identity = R identity identity
    (R v e) *> (R v' e') = R (v*>v') (e*>e')

faceNames = "LRFBUD" -- in order
faceNumber name = fromJust $ elemIndex name faceNames
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

-- Returns the edges for the given face number, in clockwise order.
edges face = let (dim, side) = faceParts face
                 d1 = ((dim+1) `mod` 3) `shiftL` 2
                 d2 = ((dim+2) `mod` 3) `shiftL` 2
             in if side == 0
                then [d1+0, d2+0, d1+1, d2+2]
                else [d1+3, d2+1, d1+2, d2+3]

-- 0,0: 0100 1000 0101 1010
-- 0,1: 0111 1001 0110 1011
-- 1,0: 1000 0000 1001 0010
-- 1,1: 1011 0001 1010 0011
-- 2,0: 0000 0100 0001 0110
-- 2,2: 0011 0101 0010 0111


-- Vertex numbers:    Edge numbers:
--    2-----3          *--1--*
--   /|    /|         4|    6|
--  0-----1 |        *--0--* | b
--  | 6---|-7      8 | *--3|-*
--  |/    |/         |5    |7
--  4-----5          *--2--*




-- The Rubik permutation of a face rotated clockwise 90 degrees.
faceMove face = let (dim, side) = faceParts face
                    d1 = (dim+1) `mod` 3
                    d2 = (dim+2) `mod` 3
                    vt = fromIndexCycle [d1, d2]
                in  R (fromCycle vt (vertices face))
                        (fromCycle (-1::Int) (edges face))

moves = array ((0,1),(5,3)) [((f,n),mv f n) | f <- [0..5], n <- [1..3]]
    where mv f n = ntimes (faceMove f) n
          ntimes m 1 = m
          ntimes m n = m *> ntimes m (n-1)

rubik [] = identity
rubik (c:cs) = rubik' (faceNumber c) cs
    where rubik' face (c:cs) = (moves ! (face, read [c])) *> rubik cs

movesOnlyBottom (R v e) = v `leavesUnmoved` nonDownVertices &&
                          e `leavesUnmoved` nonDownEdges
    where nonDownVertices = [0..7] \\ vertices 5 -- 5 is the bottom face
          nonDownEdges = [0..11] \\ edges 5


findSequence :: RandomGen g => g -> ([(Int, Int)], g)
findSequence g = fs g 1 [first] False (moves!first)
    where first = (2, 3) -- WLOG, start with F3
          maxlen = 20
          fs g n seq@((lastFace,_):_) opp perm
              | n > maxlen = ([], g)
              | movesOnlyBottom perm = (reverse seq, g)
              | otherwise = let (next@(nextFace,_), g') = genMove g lastFace opp
                                opp' = (nextFace `isOpposite` lastFace)
                                perm' = moves!next
                            in fs g' (n+1) (next:seq) opp' (perm *> perm')
          genMove g face opp =
              let (nf, g') = randomR (0,5) g
              in if nf == face || opp && nf == oppositeFace face
                 then genMove g' face opp
                 else let (rot, g'') = randomR (1,3) g'
                      in ((nf, rot), g'')

main = do
  g <- getStdGen
  findSequences 0 g
      where
        findSequences n g = do
          let (seq, g') = findSequence g
          printNonEmpty seq
          printCount n
          findSequences (n+1) g'
        printNonEmpty [] = return ()
        printNonEmpty seq = do let str = toString seq
                               putStrLn (str ++ "  " ++ show (rubik str))
        toString [] = ""
        toString ((f,r):seq) = ((faceNames !! f):show r) ++ toString seq
        printCount n
            | n `mod` 10000 == 0 = do putStrLn $ show n
            | otherwise = return ()
