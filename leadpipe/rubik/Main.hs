module Main where

import Rubik

import Data.Array
import qualified Data.Set as Set
import System.Random

findSequence :: RandomGen g => g -> Set.Set [(Int, Int)] -> ([(Int, Int)], g)
findSequence g s = fs g 1 [first] False (movePerms!first)
    where first = (2, 3) -- WLOG, start with F3
          maxlen = 20
          fs g n seq@((lastFace,_):_) opp perm
              | n > maxlen
                  = ([], g)
              | movesOnlyBottom perm
                  = (reverse seq, g)
              | otherwise
                  = let (next@(nextFace,_), g') = genMove g lastFace opp
                        opp' = (nextFace `isOpposite` lastFace)
                        perm' = movePerms!next
                        a@(ms, g'') = fs g' (n+1) (next:seq) opp' (perm *> perm')
                    in if null ms || fst(random g) || not (Set.member ms s)
                       then a
                       else fs g'' n seq opp perm -- backtrack and try again
          genMove g face opp =
              let (nf, g') = randomR (0,5) g
              in if nf == face || opp && nf == oppositeFace face
                 then genMove g' face opp
                 else let (rot, g'') = randomR (1,3) g'
                      in ((nf, rot), g'')

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
          let moveSeqs = map (toMoves'.head.words) lines
          return (Set.fromList moveSeqs)
