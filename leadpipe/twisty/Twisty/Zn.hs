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

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Defines the Zn type: integers mod n.  Sometimes known as Z/nZ.  This is a
-- group under addition.
module Twisty.Zn
       -- * The Zn type
       ( Zn()
         -- * The Nat classes
       , Nat(..)
       , NatShow(..)
       , NatRead(..)
       )
where

import Twisty.Group

import Data.Ix (Ix(..))
import Data.Monoid (Monoid, mappend, mempty)
import GHC.Enum (boundedEnumFrom, boundedEnumFromThen)
import Numeric (showInt)


-- | Type class for natural numbers.
class Nat n where
  -- | Produces the integral value associated with the type.  This must be a
  -- non-strict function: Zn will call this with an undefined arguments.
  toInt :: Integral i => n -> i

-- | Type class for displaying natural numbers.
class NatShow n where
  -- | Shows an int in a way appropriate for the type.  Non-strict in the first
  -- argument.  Default shows it in the usual way.
  showsInt :: Integral i => n -> i -> ShowS
  showsInt _ = shows

instance Nat n => NatShow n

-- | Type class for parsing natural numbers.
class NatRead n where
  -- | Reads an int in a way appropriate for the type.  Non-strict in the first
  -- argument.  Default reads it in the usual way.
  readsInt :: (Integral i, Read i) => n -> ReadS i
  readsInt _ = reads

instance Nat n => NatRead n


-- | The natural numbers mod n.  The type parameter must be an instance of Nat:
-- that is, it must have a natural number n associated with the type.
newtype Zn n = Zn Int deriving (Eq, Ord)

instance Nat n => Num (Zn n) where
  Zn x + Zn y = Zn $ (x+y) `mod` toInt (undefined :: n)
  negate (Zn 0) = 0
  negate (Zn x) = Zn $ toInt (undefined :: n) - x
  Zn x * Zn y = Zn $ (x*y) `mod` toInt (undefined :: n)
  fromInteger n = Zn $ fromInteger n `mod` toInt (undefined :: n)
  -- | Not well defined for these types.
  abs = id
  -- | Not well defined for these types.
  signum (Zn 0) = 0
  signum _ = 1

instance NatShow n => Show (Zn n) where
  showsPrec _ (Zn x) = showsInt (undefined::n) x

instance NatRead n => Read (Zn n) where
  readsPrec _ s = map f $ readsInt (undefined::n) s
    where f (x, s) = (Zn x, s)

instance Nat n => Real (Zn n) where
  toRational (Zn n) = toRational n

instance Nat n => Integral (Zn n) where
  (Zn n1) `quotRem` (Zn n2) = (Zn q, Zn r)
    where (q, r) = n1 `quotRem` n2
  toInteger (Zn n) = toInteger n

instance Nat n => Enum (Zn n) where
  toEnum i = Zn $ i `mod` toInt (undefined::n)
  fromEnum (Zn n) = n
  enumFrom = boundedEnumFrom
  enumFromThen = boundedEnumFromThen

instance Nat n => Bounded (Zn n) where
  minBound = 0
  maxBound = Zn $ toInt (undefined::n) - 1

instance Nat n => Ix (Zn n) where
  range (l, u) = [l..u]
  index (l, u) e = fromEnum e - fromEnum l
  inRange (l, u) e = e >= l && e <= u

instance Nat n => Monoid (Zn n) where
  mempty = 0
  mappend = (+)

instance Nat n => Group (Zn n) where
  ginvert = negate


-- A simple Zn type (ie integers mod n) can be had like this:
--   data T33
--   instance Nat T33 where toInt _ = 33
--   type Z33 = Zn T33
--
-- Then a value of type Z33 will be printed as an integer in the range 0..32;
-- adding two of them will produce a new one in the same range, wrapping as
-- needed.
