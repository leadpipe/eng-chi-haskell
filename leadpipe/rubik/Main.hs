module Main where

import Rubik

import Data.Array
import qualified Data.Set as Set
import System.Random

findSequence :: RandomGen g => g -> ([(Int, Int)], g)
findSequence g = fs g 1 [first] False (rubikPerms!first)
    where first = (2, 3) -- WLOG, start with F3
          maxlen = 20
          fs g n seq@((lastFace,_):_) opp perm
              | n > maxlen = ([], g)
              | movesOnlyBottom perm = (reverse seq, g)
              | otherwise = let (next@(nextFace,_), g') = genMove g lastFace opp
                                opp' = (nextFace `isOpposite` lastFace)
                                perm' = rubikPerms!next
                            in fs g' (n+1) (next:seq) opp' (perm *> perm')
          genMove g face opp =
              let (nf, g') = randomR (0,5) g
              in if nf == face || opp && nf == oppositeFace face
                 then genMove g' face opp
                 else let (rot, g'') = randomR (1,3) g'
                      in ((nf, rot), g'')

main = do
  g <- getStdGen
  findSequences 0 g Set.empty
      where
        findSequences n g s = do
          let (seq, g') = findSequence g
              s' = Set.insert seq s
          if n `mod` 100000 == 0
            then putStrLn (show n)
            else return ()
          if null seq || Set.member seq s
            then return ()
            else do
              let str = rubikString seq
              putStrLn (str ++ "  " ++ show (rubik str))
          findSequences (n+1) g' s'
