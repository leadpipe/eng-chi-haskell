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

-- | Defines the Nat class, which lets us identify a type with a natural number,
-- which in turn makes it possible to have a type corresponding to the integers
-- mod that number.
module Twisty.Nat where

import Twisty.Group

import Data.Ix (Ix(..))
import Data.Monoid (Monoid, mappend, mempty)
import GHC.Enum (boundedEnumFrom, boundedEnumFromThen)
import Numeric (showInt)


-- | This class lets us easily define groups corresponding to integers mod any
-- number.
class Nat a where
  value :: a -> Int
  showsInt :: a -> Int -> ShowS
  showsInt _ = shows
  readsInt :: a -> ReadS Int
  readsInt _ = reads

newtype Zn n = Zn Int deriving (Eq, Ord)

instance Nat n => Show (Zn n) where
  showsPrec _ (Zn x) = showsInt (undefined::n) x

instance Nat n => Read (Zn n) where
  readsPrec _ s = map f $ readsInt (undefined::n) s
    where f (x, s) = (Zn x, s)

instance Nat n => Num (Zn n) where
  Zn x + Zn y = Zn $ (x+y) `mod` value (undefined :: n)
  negate (Zn 0) = 0
  negate (Zn x) = Zn $ value (undefined :: n) - x
  Zn x * Zn y = Zn $ (x*y) `mod` value (undefined :: n)
  fromInteger n = Zn $ fromInteger n `mod` value (undefined :: n)
  -- | Not well defined for these types.
  abs = id
  -- | Not well defined for these types.
  signum (Zn 0) = 0
  signum _ = 1

instance Nat n => Real (Zn n) where
  toRational (Zn n) = toRational n

instance Nat n => Integral (Zn n) where
  (Zn n1) `quotRem` (Zn n2) = (Zn q, Zn r)
    where (q, r) = n1 `quotRem` n2
  toInteger (Zn n) = toInteger n

instance Nat n => Enum (Zn n) where
  toEnum i = Zn $ i `mod` value (undefined::n)
  fromEnum (Zn n) = n
  enumFrom = boundedEnumFrom
  enumFromThen = boundedEnumFromThen

instance Nat n => Bounded (Zn n) where
  minBound = 0
  maxBound = Zn $ value (undefined::n) - 1

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
--   instance Nat T33 where value _ = 33
--   type Z33 = Zn T33
--
-- Then a value of type Z33 will be printed as an integer in the range 0..32;
-- adding two of them will produce a new one in the same range, wrapping as
-- needed.
