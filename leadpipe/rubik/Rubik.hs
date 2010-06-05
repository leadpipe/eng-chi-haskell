{-# LANGUAGE ScopedTypeVariables, TypeSynonymInstances, EmptyDataDecls #-}
module Rubik where

import Data.Array (Array, Ix, elems, array, (//), (!))
import Data.Bits
import Data.Char
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid


-- Monoids are useful for expressing the transformations that
-- Rubik-type puzzle pieces undergo.  A monoid is just like a group
-- but without the notion of an inverse.

-- We define some operators to make it easier to work with monoids as
-- group-like types.  We define "one" as a synonym for mempty; the *>
-- operator is our synonym for mappend; and we define ^> to be the
-- result of raising a monoid element to an integral power.

one :: Monoid m => m
one = mempty
(*>) :: Monoid m => m -> m -> m
(*>) = mappend
-- This version of ^> is copied from Data.Monoid.Combinators.replicate
-- in the monoids Hackage package.
(^>) :: (Monoid m, Integral n) => m -> n -> m
base ^> exp
  | exp < 0   = error "Negative exponent"
  | exp == 0  = one
  | otherwise = f base exp
    where
      f base exp
        | exp == 1  = base
        | even exp  = f (base *> base) (exp `quot` 2)
        | otherwise = g (base *> base) (exp `quot` 2) base
      g base exp acc
        | exp == 1  = base *> acc
        | even exp  = g (base *> base) (exp `quot` 2) acc
        | otherwise = g (base *> base) (exp `quot` 2) (base *> acc)


-- We need Z2 and Z3 as monoids, and borrowing from polyonimo we do
-- some type trickery:
class IntegerAsType a where
    value :: a -> Integer

newtype Zn n = Zn Integer deriving (Eq, Ord)

instance Show (Zn n) where
   show (Zn x) = show x

instance IntegerAsType n => Num (Zn n) where
    Zn x + Zn y = Zn $ (x+y) `mod` value (undefined :: n)
    negate (Zn 0) = 0
    negate (Zn x) = Zn $ value (undefined :: n) - x
    Zn x * Zn y = Zn $ (x*y) `mod` value (undefined :: n)
    fromInteger n = Zn $ n `mod` value (undefined :: n)
    -- | Not well defined
    abs = id
    -- | Not well defined
    signum (Zn 0) = 0
    signum _ = 1

instance IntegerAsType n => Monoid (Zn n) where
    mempty = 0
    mappend = (+)

data T2
instance IntegerAsType T2 where value _ = 2

data T3
instance IntegerAsType T3 where value _ = 3

data T4
instance IntegerAsType T4 where value _ = 4

data T5
instance IntegerAsType T5 where value _ = 5

type Z2 = Zn T2
type Z3 = Zn T3
type Z4 = Zn T4
type Z5 = Zn T5

-- This is why we need these monoids:
type EdgeFlip = Z2
type VertexTwist = Z3
type NoFaceTwist = ()
type TriangleFaceTwist = Z3
type SquareFaceTwist = Z4
type PentagonFaceTwist = Z5


-- | A "wreath product" is a way to factor a group into two parts, a
-- permutation and some other subgroup.  For example, the corner
-- pieces of a Rubik's cube are permuted by each move, but they are
-- also twisted.

-- | Our Wreath type is based on Polyonimo permutation code:
--   http://www.polyomino.f2s.com/david/haskell/hs/PermutationGroups.hs.txt
-- We use 0-based indexes, and add a twist monoid to each step.

newtype (Ord t, Monoid t) => Wreath t = Wreath [WreathMove t]

-- | A WreathMove combines an int index with a twist monoid.
data (Ord t, Monoid t) => WreathMove t = WM Int t deriving (Eq, Ord)

instance (Monoid t, Ord t, Show t) => Show (WreathMove t) where
    show (WM i t) = show i ++ "~" ++ show t

getIndex :: (Monoid t, Ord t) => WreathMove t -> Int
getIndex (WM i t) = i


-- | Look up the move a wreath applies to an index.
getWreathMove :: (Ord t, Monoid t) => Wreath t -> Int -> WreathMove t
getWreathMove (Wreath ms) i = ms `lookup` i
  where lookup (m:ms) 0 = m
        lookup (m:ms) j = lookup ms (j-1)
        lookup [] _ = WM i one -- If the index isn't there, it's not moved

-- | Chain a move through a wreath.
chainWreathMove :: (Ord t, Monoid t) => Wreath t -> WreathMove t -> WreathMove t
chainWreathMove w (WM i t) = let (WM i' t') = getWreathMove w i in WM i' (t *> t')


-- | Wreaths are also Monoids.
instance (Ord t, Monoid t) => Monoid (Wreath t) where
    mempty = Wreath []
    mappend (Wreath ms) w@(Wreath ns) = Wreath (map (chainWreathMove w) ms')
      where ms' = ms ++ [WM i one | i <- [length ms..length ns - 1]]

instance (Ord t, Monoid t) => Eq (Wreath t) where
  Wreath ms == Wreath ns = eqw 0 ms ns
    where eqw i (m:ms) (n:ns) = m == n && eqw (i+1) ms ns
          eqw _ [] [] = True
          eqw i (m:ms) [] = m == WM i one && eqw (i+1) ms []
          eqw i [] (n:ns) = n == WM i one && eqw (i+1) [] ns

instance (Ord t, Monoid t) => Ord (Wreath t) where
    compare w@(Wreath ms) x@(Wreath ns) = if w == x then EQ else compare ms ns


-- Specific wreath types
type VertexWreath = Wreath VertexTwist
type EdgeWreath = Wreath EdgeFlip


-- | Tells whether a wreath leaves a (sorted) list of indices alone.
leavesUnmoved (Wreath list) indices = f 0 indices list
    where f _ [] _ = True
          f _ _ [] = True
          f j (i:is) (m:ms)
            | j == i    = m == WM i one && f (j+1) is ms
            | otherwise = f (j+1) (i:is) ms

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


showEdgeFlip 0 = id
showEdgeFlip 1 = showChar '+'

showVertexTwist 0 = id
showVertexTwist 1 = showChar '+'
showVertexTwist 2 = showChar '-'


-- | A Move of a Rubik-style puzzle twists one or more layers of a face.
data Move =
  Move { getFace :: !Int   -- ^ The face to twist
       , getLayers :: !Int -- ^ A bit set of the layers to twist
       , getTwist :: !Int  -- ^ The number of clockwise increments to twist
       }
  deriving (Eq, Ord)


-- | A Rubik type embodies a particular Rubik-style puzzle.
class Monoid r => Rubik r where
  -- | Constructs an instance from a single clockwise twist of one
  -- layer of one face.  The first argument is the face number, the
  -- second is the layer number.
  fromFaceTwist :: Int -> Int -> r

  -- | Constructs an instance with just a single move from solved.
  -- Default implementation calls 'fromFaceTwist'.
  fromMove :: Move -> r
  fromMove m = mconcat layerMoves ^> getTwist m
    where layerMoves = [fromFaceTwist (getFace m) layer |
                        layer <- layers 0 (getLayers m)]
          layers i n
            | n <= 0    = []
            | otherwise = let rest = layers (i+1) (n`shiftR`1)
                          in if n`testBit`0 then i : rest else rest


data (Rubik r) => RubikPath r = RubikPath [Move] r

instance (Rubik r) => Monoid (RubikPath r) where
  mempty = RubikPath [] mempty
  mappend (RubikPath ms1 r1) (RubikPath ms2 r2) =
    RubikPath (ms2 *> ms1) (r1 *> r2) -- Note the move list is backward

applyMove :: (Rubik r) => RubikPath r -> Move -> RubikPath r
applyMove (RubikPath ms r) m = RubikPath (m:ms) (r *> fromMove m)


-- | Gives us type safety for vertex numbers.
newtype Vertex = Vertex Int deriving (Eq, Ord, Ix, Read, Show)

-- | Gives us type safety for edge numbers.
newtype Edge = Edge Int deriving (Eq, Ord, Ix, Read, Show)
