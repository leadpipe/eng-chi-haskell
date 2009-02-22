{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Heap (
   HeapAddr,
   Heap,
   hEmpty,
   hSize,
   hAddrs,
   hAlloc,
   hLookup,
   hUpdate,
   hFree,
   PJLHeap,
   MHeap,
   ) where

import Control.Monad.Error
import Data.List as L
import Data.Map as M

newtype HeapAddr = HeapAddr Int
  deriving (Enum,Eq,Ord,Show)

class Heap h where
  hEmpty :: h a
  hSize  :: h a -> Int
  hAddrs :: h a -> [HeapAddr]
  hAlloc :: h a -> a -> (h a, HeapAddr)
  hLookup :: Error e => h a -> HeapAddr -> Either e a
  hUpdate :: Error e => h a -> HeapAddr -> a -> Either e (h a)
  hFree   :: Error e => h a -> HeapAddr -> Either e (h a)

  -- Instances probably want a more efficient version than this.
  hSize = L.length . hAddrs


{-| PJLHeap is an association list based heap.
Based upon work from "Implementing Functional Languages: a tutorial"
by: Simon Peyton-Jons, David Lester -}
data PJLHeap a = PJLHeap !Int [HeapAddr] (PJLHContents a)
type PJLHContents a = [(HeapAddr, a)]

instance Heap PJLHeap where
  -- | hEmpty is /O(1)/
  hEmpty = PJLHeap 0 [HeapAddr 1..] []
  -- | hSize is /O(1)/
  hSize (PJLHeap s _ _)  = s
  -- | hAddrs is /O(n)/
  hAddrs (PJLHeap _ _ xs)  = L.map fst xs

  -- | hAlloc is /O(1)/
  hAlloc (PJLHeap s (k:ks) xs) v = (h, k)
    where h = (PJLHeap (s+1) ks ((k,v) : xs))

  -- | hLookup is /O(n)/
  hLookup (PJLHeap _ _ xs) k
    = case L.lookup k xs of
      Nothing  -> fail ("Attempt to lookup nonexistent address #" ++ show k)
      (Just v) -> return v

  -- | hUpdate is /O(n)/
  hUpdate (PJLHeap s ks xs) k v = do
    xs' <- pjlRemove "Attempt to update nonexistent address #" k xs
    return $ PJLHeap s ks ((k,v) : xs')
  
  -- | hFree is /O(n)/
  hFree (PJLHeap s ks xs) k = do
    xs' <- pjlRemove "Attempt to free nonexistent address #" k xs
    return $ PJLHeap (s-1) (k:ks) xs'

pjlRemove :: Error e => String -> HeapAddr -> (PJLHContents a)
    -> Either e (PJLHContents a)
pjlRemove msg k [] = fail (msg ++ show k)
pjlRemove msg k (x@(k',_):xs)
  | k == k' = return xs
  | otherwise = pjlRemove msg k xs >>= return . (x:)


{-| MHeap is a Map-based Heap. -}
data MHeap a = MHeap [HeapAddr] (MHContents a)
type MHContents a = Map HeapAddr a

instance Heap MHeap where
  -- | hEmpty is /O(1)/
  hEmpty = MHeap [HeapAddr 1..] M.empty
  -- | hSize is /O(1)/
  hSize (MHeap _ m) = M.size m
  -- | hAddrs is /O(n)/
  hAddrs (MHeap _ m) = M.keys m

  -- | hAlloc is /O(log n)/
  hAlloc (MHeap (k:ks) m) v = (h, k)
    where h = MHeap ks (M.insert k v m)

  -- | hLookup is /O(log n)/
  hLookup (MHeap _ m) k
    = case M.lookup k m of
      Nothing  -> fail ("Attempt to lookup nonexistent address #" ++ show k)
      (Just v) -> return v

  -- | hUpdate is /O(log n)/
  hUpdate (MHeap s m) k v = do
    m' <- mhUpdate "Attempt to update nonexistent address #" k (Just v) m
    return $ MHeap s m'

  -- | hFree is /O(log n)/
  hFree (MHeap s m) k = do
    m' <- mhUpdate "Attempt to remove nonexistent address #" k (Nothing) m
    return $ MHeap s m'

mhUpdate :: Error e => String -> HeapAddr -> Maybe a -> MHContents a
    -> Either e (MHContents a)
mhUpdate msg k v m
  = case M.updateLookupWithKey (const $ const $ v) k m of
      (Nothing, _) -> fail (msg ++ show k)
      (_, m') -> return $ m'

