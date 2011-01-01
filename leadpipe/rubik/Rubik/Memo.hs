{-# LANGUAGE ScopedTypeVariables #-}
-- | Building on Data.MemoCombinators.
module Rubik.Memo where

import Data.Ix (Ix)
import Data.MemoCombinators (Memo, unsafeArrayRange)

array :: forall a. (Bounded a, Ix a) => Memo a
array = unsafeArrayRange (minBound, maxBound)
