{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Rubik.Cube
import Rubik.Cube4
import Rubik.Cycles
import Rubik.Puzzle
import Rubik.Searching

import Control.Monad.Random
import Data.Array.IArray
import Data.List (sort)
import System.Random

main = do
  let initialMoves = map read ["f+", "F+", "f=", "F="] :: [FaceTwist4]
  algs <- evalRandIO $ findAlgorithms initialMoves movesTopEdges (fanOut 2 20) genMove
  sequence_ $ map (putStrLn . show) algs


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
  if i <= 3 then randomMove else
    let closeOuts = calcCloseOuts alg
        numCloseOuts = length closeOuts
    in if numCloseOuts == 0 then randomMove else
         do j <- getRandomR (0, numCloseOuts - 1)
            return (closeOuts !! j)
    where randomMove = do
            f <- getRandomR (fromEnum (minBound::Face), fromEnum (maxBound::Face))
            b <- getRandom
            t <- getRandomR (1,3)
            return (FT4 (toEnum f) b (fromInteger t))


calcCloseOuts :: Algorithm Cube4 -> [FaceTwist4]
calcCloseOuts alg = [FT4 f b (-t) | (i@(f, b), t) <- assocs twists, t /= 0, i /= lastIndex]
  where lastIndex = (lf, lb)
        (FT4 lf lb _) = lastMove alg
        twists :: Array (Face, Bool) Twist4
        twists = accumArray (+) 0 ((minBound, minBound), (maxBound, maxBound)) ivs
        ivs = [((f, b), t) | (FT4 f b t) <- moves alg]
