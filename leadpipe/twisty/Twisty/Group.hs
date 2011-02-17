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

-- | Defines the 'Group' class as an extension of the standard 'Monoid' class.
-- Extends several of the standard Monoid instances to be Groups.  Defines some
-- synonyms and operators to make it easier to work with monoids and groups.
module Twisty.Group where

import Data.Monoid (Monoid, mappend, mempty, Dual(..), Sum(..))

-- | We extend Monoid to make a group class, by adding an inverse operator.
class (Monoid a) => Group a where
  ginvert :: a -> a
  -- ^ Returns the inverse of the given group element, ie the element that
  -- yields mempty when mappend'ed to the original element.


-- | This is a synonym for 'mempty', using a common group-theoretic name for the
-- group's identity.
one :: Monoid m => m
one = mempty

-- | This is a synonym for 'mappend', the group's operation.  The mnemonic here
-- is that a group's operation is sort of a cross between multiplication and
-- function application.
($*) :: Monoid m => m -> m -> m
($*) = mappend
infixl 7 $* -- matches (*)

-- | Raises a group element to an integral power, ie applying it to itself n
-- times, where n can also be 0 (returns the identity) or negative (uses the
-- inverse).
($^) :: (Group g, Integral n) => g -> n -> g
infixr 8 $^ -- matches (^)
-- This version of $^ is copied from Data.Group.Combinators.replicate in the
-- "monoids" package.
base $^ exp
  | exp < 0   = ginvert base $^ negate exp
  | exp == 0  = one
  | otherwise = f base exp
    where
      f base exp
        | exp == 1  = base
        | even exp  = f (base $* base) (exp `quot` 2)
        | otherwise = g (base $* base) (exp `quot` 2) base
      g base exp acc
        | exp == 1  = base $* acc
        | even exp  = g (base $* base) (exp `quot` 2) acc
        | otherwise = g (base $* base) (exp `quot` 2) (base $* acc)


-- Some Group defs corresponding to (some of) the basic Monoid defs:
instance (Group b) => Group (a -> b) where
  ginvert f x = ginvert (f x)

instance Group () where
  ginvert () = ()

instance (Group a, Group b) => Group (a, b) where
  ginvert (a, b) = (ginvert a, ginvert b)

instance (Group a, Group b, Group c) => Group (a, b, c) where
  ginvert (a, b, c) = (ginvert a, ginvert b, ginvert c)

instance (Group a, Group b, Group c, Group d) => Group (a, b, c, d) where
  ginvert (a, b, c, d) = (ginvert a, ginvert b, ginvert c, ginvert d)

instance (Group a, Group b, Group c, Group d, Group e) => Group (a, b, c, d, e) where
  ginvert (a, b, c, d, e) = (ginvert a, ginvert b, ginvert c, ginvert d, ginvert e)

instance (Group a) => Group (Dual a) where
  ginvert (Dual x) = Dual (ginvert x)

instance (Num a) => Group (Sum a) where
  ginvert (Sum x) = Sum (negate x)


-- | The conjugate of y according to x.
conjugate :: (Group g) => g -> g -> g
conjugate x y = x $* y $* ginvert x

-- | An operator for conjugate.  Mnemonic: normal group operation on the
-- arguments followed by appending the inverse of the first argument.
($*~) :: (Group g) => g -> g -> g
($*~) = conjugate
infixl 6 $*~    -- matches +, lower than $*

-- | The commutator of x and y.
commutator :: (Group g) => g -> g -> g
commutator x y = x $* y $* ginvert x $* ginvert y

-- | An operator for commutator.  Mnemonic: normal group operation on the
-- arguments followed by appending the inverses of both arguments.
($*~~) :: (Group g) => g -> g -> g
($*~~) = commutator
infixl 6 $*~~   -- matches +
