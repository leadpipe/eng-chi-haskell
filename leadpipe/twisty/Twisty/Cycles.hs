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

{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
module Twisty.Cycles where

import Twisty.Group
import Twisty.Lists
import Twisty.Wreath

import Data.List (elemIndex)
import Data.Maybe (fromJust)


-- | Creates a cycle from a face, a list of pieces, and a function that maps a
-- piece to its constituent faces.  Calculates the twist for each piece as the
-- difference in location of the face within each pair of pieces' faces.
asCycle :: forall f a. (Eq f, WreathPermutable a, Num (WreathTwist a)) =>
           f -> [a] -> (a -> [f]) -> [WreathEntry a]
asCycle f as toFs = zipWith toEntry as twists
  where toEntry a t = Entry (a, t)
        indices = map indexIn as
        indexIn a = toInteger $ fromJust $ f `elemIndex` toFs a
        twists = zipWith toTwist indices $ rotate 1 indices
        toTwist i j = fromInteger (i - j)


-- | A variant of asCycle for when the list of pieces is more readily accessible
-- as a function of the face.
asCycle' :: forall f a. (Eq f, WreathPermutable a, Num (WreathTwist a)) =>
            f -> (f -> [a]) -> (a -> [f]) -> [WreathEntry a]
asCycle' f toAs toFs = asCycle f (toAs f) toFs


-- | Creates a cycle from a list of pieces, for situations where no twisting is
-- possible.
asSimpleCycle :: forall a. (WreathPermutable a) => [a] -> [WreathEntry a]
asSimpleCycle as = map toEntry as
  where toEntry a = Entry (a, one)
