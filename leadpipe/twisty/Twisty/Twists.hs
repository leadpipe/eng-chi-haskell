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

{-# LANGUAGE EmptyDataDecls #-}

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

import Twisty.Zn


-- | Twist type corresponding to natural number 1.
data Tw1
instance Nat Tw1 where
  toInt _ = 1
instance NatShow Tw1 where
  showsInt _ _ = id
instance NatRead Tw1 where
  readsInt _ s = [(0, s)]

-- | Twist type corresponding to natural number 2.
data Tw2
instance Nat Tw2 where
  toInt _ = 2
instance NatShow Tw2 where
  showsInt _ = showTwist
    where showTwist 0 = id
          showTwist 1 = showChar '+'
instance NatRead Tw2 where
  readsInt _ = readTwist
    where readTwist ('+':s) = [(1, s)]
          readTwist s = [(0, s)]

-- | Twist type corresponding to natural number 3.
data Tw3
instance Nat Tw3 where
  toInt _ = 3
instance NatShow Tw3 where
  showsInt _ = showTwist
    where showTwist 0 = id
          showTwist 1 = showChar '+'
          showTwist 2 = showChar '-'
instance NatRead Tw3 where
  readsInt _ = readTwist
    where readTwist ('+':s) = [(1, s)]
          readTwist ('-':s) = [(2, s)]
          readTwist s = [(0, s)]

-- | Twist type corresponding to natural number 4.
data Tw4
instance Nat Tw4 where
  toInt _ = 4
instance NatShow Tw4 where
  showsInt _ = showTwist
    where showTwist 0 = id
          showTwist 1 = showChar '+'
          showTwist 2 = showChar '='
          showTwist 3 = showChar '-'
instance NatRead Tw4 where
  readsInt _ = readTwist
    where readTwist ('+':s) = [(1, s)]
          readTwist ('=':s) = [(2, s)]
          readTwist ('-':s) = [(3, s)]
          readTwist s = [(0, s)]

-- | Twist type corresponding to natural number 5.
data Tw5
instance Nat Tw5 where
  toInt _ = 5
instance NatShow Tw5 where
  showsInt _ = showTwist
    where showTwist 0 = id
          showTwist 1 = showChar '+'
          showTwist 2 = showString "++"
          showTwist 3 = showString "--"
          showTwist 4 = showChar '-'
instance NatRead Tw5 where
  readsInt _ = readTwist
    where readTwist ('+':'+':s) = [(2, s)]
          readTwist ('+':s) = [(1, s)]
          readTwist ('-':'-':s) = [(3, s)]
          readTwist ('-':s) = [(4, s)]
          readTwist s = [(0, s)]

-- | Twist type corresponding to natural number 6.
data Tw6
instance Nat Tw6 where
  toInt _ = 6
instance NatShow Tw6 where
  showsInt _ = showTwist
    where showTwist 0 = id
          showTwist 1 = showChar '+'
          showTwist 2 = showString "++"
          showTwist 3 = showChar '='
          showTwist 4 = showString "--"
          showTwist 5 = showChar '-'
instance NatRead Tw6 where
  readsInt _ = readTwist
    where readTwist ('+':'+':s) = [(2, s)]
          readTwist ('+':s) = [(1, s)]
          readTwist ('=':s) = [(3, s)]
          readTwist ('-':'-':s) = [(4, s)]
          readTwist ('-':s) = [(5, s)]
          readTwist s = [(0, s)]

type Twistless = Zn Tw1 -- ^ Trivial twist group.
type Flip = Zn Tw2      -- ^ Order-2 twist group.
type Twist3 = Zn Tw3    -- ^ Order-3 twist group.
type Twist4 = Zn Tw4    -- ^ Order-4 twist group.
type Twist5 = Zn Tw5    -- ^ Order-5 twist group.
type Twist6 = Zn Tw6    -- ^ Order-6 twist group.
