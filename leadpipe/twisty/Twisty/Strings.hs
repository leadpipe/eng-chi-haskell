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
fromOptS def = maybe def appEndo
