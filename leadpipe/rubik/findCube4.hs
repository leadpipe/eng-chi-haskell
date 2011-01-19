{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Rubik.Cube
import Rubik.Cube4
import Rubik.Cycles
import Rubik.Puzzle
import Rubik.Searching

import Control.Monad.Random
import Data.List (sort)
import System.Random

main = do
  let initialMoves = map read ["f+", "F+", "f=", "F="] :: [FaceTwist4]
  algs <- evalRandIO $ findAlgorithms initialMoves movesTopEdges (fanOut 2 20) genMove
  sequence_ [putStrLn (show alg) | alg <- algs]


-- | Tells whether the given algorithm moves edge pieces only on the top (U)
-- face.  It ignores what the algorithm does to vertices and face pieces.
movesTopEdges :: Algorithm Cube4 -> Bool
movesTopEdges a = leavesUnmoved e nonTopEdgePieceIndices
  where Cube4 (_, e, _) = result a

nonTopEdgePieceIndices :: [Int]
nonTopEdgePieceIndices = sort [fromEnum ep | ep <- [minBound..], U `notElem` edgePieceFaces ep]


fanOut :: Int -> Int -> Algorithm Cube4 -> Int -> Bool
fanOut width depth a n = n < width && length (moves a) <= depth


-- | Generates a random move, sometimes using the given algorithm to close out
-- the cumulative twist for a face.
genMove :: Algorithm Cube4 -> Rand StdGen FaceTwist4
genMove alg = do
  (i :: Int) <- getRandomR (1, 10)
  closeOuts <- calcCloseOuts alg
  let numCloseOuts = length closeOuts
  j <- getRandomR (0, numCloseOuts)
  if i <= 7 && numCloseOuts > 0 then return (closeOuts !! j) else randomMove
    where randomMove = do
            f <- getRandomR (fromEnum (minBound::Face), fromEnum (maxBound::Face))
            b <- getRandom
            t <- getRandomR (1,3)
            return (FT4 (toEnum f) b (fromInteger t))


calcCloseOuts :: Algorithm Cube4 -> Rand StdGen [FaceTwist4]
calcCloseOuts alg = do
  return []
