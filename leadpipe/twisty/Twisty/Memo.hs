{-# LANGUAGE ScopedTypeVariables #-}
-- | Building on Data.MemoCombinators.
module Twisty.Memo (
  module Data.MemoCombinators
  , array
  ) where

import Data.Ix (Ix)
import Data.MemoCombinators

array :: forall a. (Bounded a, Ix a) => Memo a
array = unsafeArrayRange (minBound, maxBound)
