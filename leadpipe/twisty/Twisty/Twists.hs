{-
Copyright 2011 Google Inc.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

{-# LANGUAGE ScopedTypeVariables, TypeSynonymInstances, EmptyDataDecls #-}

-- | Defines some simple additive group types corresponding to Z1 through Z6
-- (the natural numbers mod 1 through 6).  These can be used to model the twists
-- that the individual pieces of a twisty puzzle undergo.
module Twisty.Twists
       ( Twistless
       , Flip
       , Twist3
       , Twist4
       , Twist5
       , Twist6
       )
where

import Twisty.Nat


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
