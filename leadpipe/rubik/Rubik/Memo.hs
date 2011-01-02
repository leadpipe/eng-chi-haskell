{-# LANGUAGE ScopedTypeVariables #-}
-- | Building on Data.MemoCombinators.
module Rubik.Memo (
  module Data.MemoCombinators
  ) where

import Data.Ix (Ix)
import Data.MemoCombinators

array :: forall a. (Bounded a, Ix a) => Memo a
array = unsafeArrayRange (minBound, maxBound)
