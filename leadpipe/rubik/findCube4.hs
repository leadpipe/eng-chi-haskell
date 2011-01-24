{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Rubik.Algebra
import Rubik.Cube
import Rubik.Cube4
import Rubik.Cycles
import Rubik.Puzzle
import Rubik.Searching

import Control.Monad.Random
import Data.Array.IArray
import Data.List (sort)
import Data.Word
import System.Random

type SearchM a = Rand StdGen a
type CumulativeTwists = Array (Face, Bool) Twist4
type Node = (Algorithm Cube4, CumulativeTwists)

main = do
  let roots = map (makeRoot . read) ["f+", "F+", "f=", "F="]
  nodes <- evalRandIO $ searchForest roots calcChildren (return . movesFewVerticesAndEdges . fst)
  sequence_ $ map (putStrLn . show . fst) nodes


makeRoot :: FaceTwist4 -> SearchM Node
makeRoot mv = return (one `applyMove` mv, emptyTwists `updateTwists` mv)

emptyTwists :: CumulativeTwists
emptyTwists = accumArray (+) 0 ((minBound, minBound), (maxBound, maxBound)) []

updateTwists :: CumulativeTwists -> FaceTwist4 -> CumulativeTwists
updateTwists ct (FT4 f b t) = accum (+) ct [((f, b), t)]

calcChildren :: Node -> SearchM [Node]
calcChildren node = do algs <- generateChildrenToLength 20 fst genMove 2 node
                       return $ map addTwists algs
                      where addTwists alg = (alg, snd node `updateTwists` lastMove alg)


movesFewVerticesAndEdges :: Algorithm Cube4 -> Bool
movesFewVerticesAndEdges a = numIndicesMoved v < 4 && numIndicesMoved e < 8
  where Cube4 (v, e, _) = result a


-- | Tells whether the given algorithm moves edge pieces only on the top (U)
-- face.  It ignores what the algorithm does to vertices and face pieces.
movesTopEdges :: Algorithm Cube4 -> Bool
movesTopEdges a = leavesUnmoved e nonTopEdgePieceIndices
  where Cube4 (_, e, _) = result a

nonTopEdgePieceIndices :: [Int]
nonTopEdgePieceIndices = sort [fromEnum ep | ep <- [minBound..], U `notElem` edgePieceFaces ep]


-- | Generates a random move, sometimes using the given algorithm to close out
-- the cumulative twist for a face.
genMove :: Node -> Rand StdGen FaceTwist4
genMove node = do
  i <- getRandomR (1::Int, 10)
  if i <= 3 then randomMove else
    let ats = actualTwists node
        numTwists = length ats
    in if numTwists == 0 then randomMove else
         do j <- getRandomR (0, numTwists - 1)
            let ((f, b), t) = ats !! j
            return (FT4 f b (-t))
    where randomMove = do
            f <- getRandomR (fromEnum (minBound::Face), fromEnum (maxBound::Face))
            b <- getRandomR (1::Int, 5) -- 20% chance of both layers, 80% outer layer only
            t <- getRandomR (1::Int, 3)
            return (FT4 (toEnum f) (b > 1) (toEnum t))


actualTwists :: Node -> [((Face, Bool), Twist4)]
actualTwists (alg, twists) = [a | a@(i@(f, b), t) <- assocs twists, t /= 0, i /= lastIndex]
  where lastIndex = (lf, lb)
        (FT4 lf lb _) = lastMove alg
