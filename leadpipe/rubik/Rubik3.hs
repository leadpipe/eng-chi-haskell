{-# LANGUAGE ScopedTypeVariables, TypeSynonymInstances #-}
module Rubik3 where

import Rubik
import Data.Array (Array, elems, array, (//), (!))
import Data.Bits
import Data.Char
import qualified Data.Map as Map
import Data.List
import Data.Maybe
import Numeric (showHex)



showVectorMove (M i t) = showsPrec 0 i . showString ((Map.!) dpByPerm t)
showEdgeMove (M i t) = showHex i . showIntTransform t

data Rubik3Permutation = R { v :: Permutation SimplePermutation,
                             e :: SimplePermutation }
                         deriving (Eq, Ord)

instance Show Rubik3Permutation where
    showsPrec _ (R v e) = showCycles showVectorMove v .
                          showString " | " .
                          showCycles showEdgeMove e

instance Transform Rubik3Permutation where
    identity = R identity identity
    (R v e) *> (R v' e') = R (v*>v') (e*>e')

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




-- The Rubik3 permutation of a face rotated clockwise 90 degrees.
faceMove face = let (dim, side) = faceParts face
                    d1 = (dim+1) `mod` 3
                    d2 = (dim+2) `mod` 3
                    vt = fromIndexCycle [d1, d2]
                in  R (fromCycle vt (vertices face))
                        (fromCycle (-1::Int) (edges face))

movePerms :: Array (Int, Int) Rubik3Permutation
movePerms = array ((0,1),(5,3))
             [((f,n),faceMove f ^> n) | f <- [0..5], n <- [1..3]]

-- Combines adjacent moves to yield a possibly shorter list.
compressMoves ms = maybe ms id (cm ms)
    where cm (m1@(f1,r1):m2@(f2,r2):ms)
              | f1 == f2
                  = let r = (r1+r2) `mod` 4 in
                    return (compressMoves (if r > 0 then ((f1,r):ms) else ms))
              | f1 `isOpposite` f2 && f1 > f2
                  = return (compressMoves (m2:m1:ms))
              | otherwise -- the do-expr skips Nothings from recursive cm
                  = do { ms' <- cm (m2:ms); return (compressMoves (m1:ms')) }
          cm _ = Nothing  -- ie, no changes

-- Converts a string to a list of (face, rotation) pairs.
toMoves' = map toMove . filter isAlpha
    where toMove c = (faceNumber (toUpper c), if isUpper c then 1 else 3)

toMoves = compressMoves . toMoves'

mirrorMoves dim = map mirrorMove
    where mirrorMove (f,r) = (index (f .^ fromIndexCycle [2*dim,2*dim+1]), 4-r)

rotateMoves dim n = map rotateMove
    where rotateMove (f,r) = (index (f .^ cycle), r)
          cycle = fromIndexCycle [f1, f2+1, f1+1, f2] ^> n
          f1 = (dim+1) `mod` 3 * 2
          f2 = (dim+2) `mod` 3 * 2

invertMoves = reverse . map im
    where im (f,r) = (f, 4-r)

evalMoves = foldl' (*>) identity . map (movePerms !)

toString = foldl' (++) "" . map rs
    where rs (f, r) = let fn = faceNames !! f in
                      case r of
                       1 -> [fn]
                       2 -> [fn, fn]
                       3 -> [toLower fn]

mirrorString dim = toString . mirrorMoves dim . toMoves
rotateString dim n = toString . rotateMoves dim n . toMoves
diagString n = toString . mirrorMoves 2 . rotateMoves 2 n . rotateMoves 0 2 . toMoves
invertString = toString . invertMoves . toMoves

-- Evaluates a string of moves, returns the rubik3 permutation resulting.
eval = evalMoves . toMoves

movesOnlyBottom (R v e) = v `leavesUnmoved` nonDownVertices &&
                          e `leavesUnmoved` nonDownEdges
    where nonDownVertices = [0..7] \\ vertices 5 -- 5 is the "down" (bottom) face
          nonDownEdges = [0..11] \\ edges 5

-- Conveniences for finding related move strings pertaining to the
-- "down" (bottom) face.
inv = invertString
rot1 = rotateString 2 1
rot2 = rotateString 2 2
rot3 = rotateString 2 3
mir0 = mirrorString 0
mir1 = mirrorString 1
diag1 = diagString 1
diag2 = diagString 3

-- Lists the related move strings.
rel s = map (flip ($) s) [id, rot1, rot2, rot3, mir0, mir1, diag1, diag2]
-- As above, plus inverses.
relInv s = rel s ++ map inv (rel s)

putStringPerm s p = putStrLn (s ++ "\t" ++ show p)
evm ms = putStringPerm (toString ms) (evalMoves ms)
evs = evm . toMoves

evss = mapM_ evs

-- Shows all pairs of one sequence with another's related sequences.
pairs s1 s2 = evss $ map (s1++) (relInv s2)

-- Shows all pairs of a given sequence with its own related sequences.
pairs_ s = pairs s s

-- Shows all triples of a given sequence with its own related sequences.
triples_ s = evss $ map concat $ sequence [map (s++) rels, rels]
    where rels = relInv s
