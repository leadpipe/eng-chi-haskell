{-# LANGUAGE ScopedTypeVariables, TypeSynonymInstances, EmptyDataDecls #-}

-- | Defines some simple additive group types corresponding to Z1 through Z6
-- (the natural numbers mod 1 through 6).  These can be used to model the twists
-- that the individual pieces of a Rubik-style puzzle undergo.
module Rubik.Twists
       ( Twistless
       , Flip
       , Twist3
       , Twist4
       , Twist5
       , Twist6
       )
where

import Rubik.Nat


data Tw1
instance Nat Tw1 where
  value _ = 1
  showsInt _ _ = id

data Tw2
instance Nat Tw2 where
  value _ = 2
  showsInt _ = showTwist
    where showTwist 0 = id
          showTwist 1 = showChar '+'
  readsInt _ = readTwist
    where readTwist ('+':s) = [(1, s)]
          readTwist s = [(0, s)]

data Tw3
instance Nat Tw3 where
  value _ = 3
  showsInt _ = showTwist
    where showTwist 0 = id
          showTwist 1 = showChar '+'
          showTwist 2 = showChar '-'
  readsInt _ = readTwist
    where readTwist ('+':s) = [(1, s)]
          readTwist ('-':s) = [(2, s)]
          readTwist s = [(0, s)]

data Tw4
instance Nat Tw4 where
  value _ = 4
  showsInt _ = showTwist
    where showTwist 0 = id
          showTwist 1 = showChar '+'
          showTwist 2 = showChar '='
          showTwist 3 = showChar '-'
  readsInt _ = readTwist
    where readTwist ('+':s) = [(1, s)]
          readTwist ('=':s) = [(2, s)]
          readTwist ('-':s) = [(3, s)]
          readTwist s = [(0, s)]

data Tw5
instance Nat Tw5 where
  value _ = 5
  showsInt _ = showTwist
    where showTwist 0 = id
          showTwist 1 = showChar '+'
          showTwist 2 = showString "++"
          showTwist 3 = showString "--"
          showTwist 4 = showChar '-'
  readsInt _ = readTwist
    where readTwist ('+':'+':s) = [(2, s)]
          readTwist ('+':s) = [(1, s)]
          readTwist ('-':'-':s) = [(3, s)]
          readTwist ('-':s) = [(4, s)]
          readTwist s = [(0, s)]

data Tw6
instance Nat Tw6 where
  value _ = 6
  showsInt _ = showTwist
    where showTwist 0 = id
          showTwist 1 = showChar '+'
          showTwist 2 = showString "++"
          showTwist 3 = showChar '='
          showTwist 4 = showString "--"
          showTwist 5 = showChar '-'
  readsInt _ = readTwist
    where readTwist ('+':'+':s) = [(2, s)]
          readTwist ('+':s) = [(1, s)]
          readTwist ('=':s) = [(3, s)]
          readTwist ('-':'-':s) = [(4, s)]
          readTwist ('-':s) = [(5, s)]
          readTwist s = [(0, s)]

type Twistless = Zn Tw1
type Flip = Zn Tw2
type Twist3 = Zn Tw3
type Twist4 = Zn Tw4
type Twist5 = Zn Tw5
type Twist6 = Zn Tw6