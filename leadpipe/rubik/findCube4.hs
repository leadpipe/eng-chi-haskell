{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Rubik.Algebra
import Rubik.Cube
import Rubik.Cube4
import Rubik.Cycles
import Rubik.Puzzle
import Rubik.Searching

import Control.Monad.Random
import Control.Parallel.Strategies
import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as Map
import System.Random

type SearchM a = Rand StdGen a
type CumulativeTwists = Map (Face, Bool) Twist4
type Node = (Algorithm Cube4, CumulativeTwists)

main = do
  let roots = cycle $ map (makeRoot . read) ["f+", "F+", "f=", "F="]
  let search = searchTree calcChildren (return . movesFewVerticesAndEdges . fst)
  gens <- stdGenList
  let nodes = concat (zipWith (evalRand . search) roots gens `using` parBuffer 3 rseq)
  sequence_ $ map (putStrLn . show . fst) nodes

stdGenList :: IO [StdGen]
stdGenList = do
  gen <- newStdGen
  return $ splits gen
    where splits gen = let (g1, g2) = split gen in g1 : splits g2

makeRoot :: FaceTwist4 -> SearchM Node
makeRoot mv = return (one `applyMove` mv, emptyTwists `updateTwists` mv)

emptyTwists :: CumulativeTwists
emptyTwists = Map.empty

updateTwists :: CumulativeTwists -> FaceTwist4 -> CumulativeTwists
updateTwists ct (FT4 f b t) = Map.alter plus (f, b) ct
  where plus = toMaybe . (+) t . fromMaybe
        fromMaybe Nothing = 0
        fromMaybe (Just t) = t
        toMaybe 0 = Nothing
        toMaybe t = Just t

calcChildren :: Node -> SearchM [Node]
calcChildren node = do algs <- generateChildrenToLength 20 fst genMove 2 node
                       return $ map addTwists algs
                      where addTwists alg = (alg, snd node `updateTwists` lastMove alg)


movesFewVerticesAndEdges :: Algorithm Cube4 -> Bool
movesFewVerticesAndEdges a = numEdges > 0 && numEdges < 4 && length mvs > 4 && hasInnerTwist && facePiecesStay
  where Cube4 (_, e, f) = result a
        numEdges = numIndicesMoved e
        mvs = moves a
        hasInnerTwist = any (\(FT4 _ b _) -> not b) mvs
        facePiecesStay = True -- TODO


-- | Tells whether the given algorithm moves edge pieces only on the top (U)
-- face.  It ignores what the algorithm does to vertices and face pieces.
movesTopEdges :: Algorithm Cube4 -> Bool
movesTopEdges a = leavesUnmoved e nonTopEdgePieceIndices
  where Cube4 (_, e, _) = result a

nonTopEdgePieceIndices :: [Int]
nonTopEdgePieceIndices = sort [fromEnum ep | ep <- [minBound..], U `notElem` edgePieceFaces ep]


-- | Generates a random move, sometimes using the given algorithm to close out
-- the cumulative twist for a face.
genMove :: Node -> SearchM FaceTwist4
genMove node@(alg, twists) = do
  let (FT4 lf lb _) = lastMove alg
  let lastIndex = (lf, lb)
  if Map.null twists || Map.size twists == 1 && Map.member lastIndex twists
    then randomMove
    else do i <- getRandomR (1::Int, 10)
            if i <= 3 then randomMove else do
              let ats = applicableTwists twists lastIndex
              j <- getRandomR (0, length ats - 1)
              let ((f, b), t) = ats !! j
              return (FT4 f b (-t))
    where randomMove = do
            f <- getRandomR (fromEnum (minBound::Face), fromEnum (maxBound::Face))
            b <- getRandomR (1::Int, 5) -- 40% chance of both layers, 60% outer layer only
            t <- getRandomR (1::Int, 3)
            return (FT4 (toEnum f) (b > 2) (toEnum t))


applicableTwists :: CumulativeTwists -> (Face, Bool) -> [((Face, Bool), Twist4)]
applicableTwists twists lastIndex = [a | a@(i, _) <- Map.toList twists, i /= lastIndex]
