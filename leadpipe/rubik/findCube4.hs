{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Rubik.Algebra
import Rubik.Cube
import Rubik.Cube4
import Rubik.Cycles
import Rubik.Puzzle
import Rubik.Searching

import Control.Monad.Random
import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as Map
import System.Random

type SearchM a = Rand StdGen a
type CumulativeTwists = Map (Face, Bool) Twist4
type Node = (Algorithm Cube4, CumulativeTwists)

main = do
  let roots = cycle $ map (makeRoot . read) ["f+", "F+", "f=", "F="]
  nodes <- evalRandIO $ searchForest roots calcChildren (return . movesFewVerticesAndEdges . fst)
  sequence_ $ map (putStrLn . show . fst) nodes


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
movesFewVerticesAndEdges a = numIndicesMoved v < 4 && numIndicesMoved e < 8 && length (moves a) > 3
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
            b <- getRandomR (1::Int, 5) -- 20% chance of both layers, 80% outer layer only
            t <- getRandomR (1::Int, 3)
            return (FT4 (toEnum f) (b > 1) (toEnum t))


applicableTwists :: CumulativeTwists -> (Face, Bool) -> [((Face, Bool), Twist4)]
applicableTwists twists lastIndex = [a | a@(i, _) <- Map.toList twists, i /= lastIndex]
