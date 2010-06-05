{-# OPTIONS_GHC -fglasgow-exts -XUndecidableInstances #-}
module Testing where

import Test.QuickCheck

instance (Enum a, Bounded a) => Arbitrary a where
  arbitrary = do n <- choose (fromEnum (minBound :: a), fromEnum (maxBound :: a) )
                 return $ toEnum n
