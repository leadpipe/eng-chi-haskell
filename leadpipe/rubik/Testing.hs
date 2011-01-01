{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, UndecidableInstances #-}
module Testing where

import Test.QuickCheck

instance forall a. (Enum a, Bounded a) => Arbitrary a where
  arbitrary = do n <- choose (fromEnum (minBound :: a), fromEnum (maxBound :: a) )
                 return $ toEnum n
