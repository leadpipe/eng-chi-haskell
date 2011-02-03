-- | String utilities.
module Twisty.Strings where

import Data.Monoid (Endo(..))

-- | A monoidal optional string showing function.  Combine with mappend.
type OptS = Maybe (Endo String)

-- | Creates an OptS from a show function.
toOptS :: ShowS -> OptS
toOptS f = Just (Endo f)

-- | Converts an OptS to a show function, given a default.
fromOptS :: ShowS -> OptS -> ShowS
fromOptS def opt = maybe def appEndo opt
