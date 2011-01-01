{-# LANGUAGE ScopedTypeVariables #-}
-- | Building on Data.MemoCombinators.
module Rubik.Memo where

import qualified Data.MemoCombinators as Memo

array :: forall a. (Bounded a, Ix a) => Memo.Memo a
array = unsafeArrayRange (minBound, maxBound)
