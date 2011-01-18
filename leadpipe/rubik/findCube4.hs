module Main where

import Rubik.Cube
import Rubik.Cube4
import Rubik.Cycles
import Rubik.Puzzle
import Rubik.Searching

import Control.Monad.Random (evalRandIO)
import Data.List (sort)
import System.Random

main = do
  let initialMoves = [read "f+", read "F+", read "f=", read "F="] :: [FaceTwist4]
  algs <- evalRandIO $ findAlgorithms initialMoves movesTopEdges fanOut2to12
  sequence_ [putStrLn (show alg) | alg <- algs]


instance Random FaceTwist4 where
  randomR ((FT4 f1 b1 t1), (FT4 f2 b2 t2)) g =
    let (f, g1) = randomR (fromEnum f1, fromEnum f2) g
        (b, g2) = randomR (fromEnum b1, fromEnum b2) g1
        (t, g3) = randomR (toInteger t1, toInteger t2) g2
    in (FT4 (toEnum f) (toEnum b) (fromInteger t), g3)

  random = randomR ((FT4 minBound minBound 1), (FT4 maxBound maxBound maxBound))

-- | Tells whether the given algorithm moves edge pieces only on the top (U)
-- face.  It ignores what the algorithm does to vertices and face pieces.
movesTopEdges :: Algorithm Cube4 -> Bool
movesTopEdges a = leavesUnmoved e nonTopEdgePieceIndices
  where Cube4 (_, e, _) = result a


nonTopEdgePieceIndices :: [Int]
nonTopEdgePieceIndices = sort [fromEnum ep | ep <- [minBound..], U `notElem` edgePieceFaces ep]


fanOut2to12 :: Algorithm Cube4 -> Int -> Bool
fanOut2to12 a n = n <= 2 && length (moves a) <= 12
