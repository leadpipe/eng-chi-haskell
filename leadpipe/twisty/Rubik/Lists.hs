-- | List utilities.
module Rubik.Lists where

-- | Rotates a list by n places.
rotate n xs = drop n xs ++ take n xs
