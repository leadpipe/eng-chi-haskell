-- | Some utilities.
module Rubik.Util where

-- | Rotates a list by n places.
rotate n xs = drop n xs ++ take n xs
