{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Rubik

import Data.Array
import qualified Data.Set as Set
import System.Random

findSequence :: RandomGen g => g -> Set.Set [(Int, Int)] -> ([(Int, Int)], g)
findSequence g s = fs g 1 [first] False (movePerms!first) counts
    where first = (2, 3) -- WLOG, start with f
          maxlen = 20
          counts = (array (0,5) [(i,0) | i <- [0..5]]) // [first]
          fs g n seq@((lastFace,_):_) opp perm counts
              | n > maxlen
                  = ([], g)
              | movesOnlyBottom perm
                  = (reverse seq, g)
              | otherwise
                  = let (next@(nextFace,_), g') = genMove g counts lastFace opp
                        opp' = (nextFace `isOpposite` lastFace)
                        perm' = movePerms!next
                        counts' = accum sum4 counts [next]
                        sum4 a b = (a+b) `mod` 4
                        a@(ms, g'') = fs g' (n+1) (next:seq) opp' (perm *> perm') counts'
                    in if null ms || fst(random g) || not (Set.member ms s)
                       then a
                       else fs g'' n seq opp perm counts -- backtrack and try again
          genMove g counts face opp =
              let (nf, g') = randomR (0,5) g
                  (r, g'') = genRotation g' counts nf
              in  if nf == face || opp && nf `isOpposite` face
                  then genMove g'' counts face opp
                  else ((nf, r), g'')
          genRotation g counts nf =
              let (p::Double, g') = random g
                  c = counts ! nf
              in  if p < 0.7 && c > 0 then (4-c, g')
                  else randomR (1,3) g'

main = do
  g <- getStdGen
  found <- readFoundFile
  findSequences 0 g found
      where
        findSequences n g s = do
          let (seq, g') = findSequence g s
              s' = Set.insert seq s
          if n `mod` 1000000 == 0
            then putStrLn (show n)
            else return ()
          if null seq || Set.member seq s
            then return ()
            else evm seq
          findSequences (n+1) g' s'

        readFoundFile = do
          lines <- readFile "found.txt" >>= return.lines
          let moveSeqs = map (toMoves.head.words) lines
          return (Set.fromList moveSeqs)
