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

{-# LANGUAGE TypeFamilies #-}

-- | Defines the \"ad-supported\" 3x3 cube puzzle with faces that have a right
-- way up.
module Twisty.Cube3a where

import Twisty.Cycles
import Twisty.Cube
import Twisty.Cube3
import Twisty.FaceTwist
import Twisty.Group
import qualified Twisty.Memo as Memo
import Twisty.Puzzle
import Twisty.Twists
import Twisty.Wreath

import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Data.Monoid (Monoid, mappend, mempty)

instance WreathPermutable Face where
  type WreathTwist Face = Twist4

newtype Cube3a = Cube3a (Cube3, Wreath Face) deriving (Eq, Ord)

instance Monoid Cube3a where
  mempty = Cube3a one
  mappend (Cube3a s1) (Cube3a s2) = Cube3a (s1 $* s2)

instance Group Cube3a where
  ginvert (Cube3a s) = Cube3a (ginvert s)

instance Puzzle Cube3a where
  type Move Cube3a = CubeMove1
  fromMove = Memo.array fromMove1
    where fromMove1 :: CubeMove1 -> Cube3a
          fromMove1 m@(FaceTwist f _ 1) = Cube3a (fromMove m, fromCycles [[Entry (f, 1)]])
          fromMove1 (FaceTwist f d n) = fromMove (FaceTwist f d 1) $^ n

instance Show Cube3a where
  showsPrec _ (Cube3a (Cube3 (v, e), f)) = fromOptCycles $ optShowCycles v $* optShowCycles e $* optShowCycles f

c3a :: String -> Algorithm Cube3a
c3a = read
