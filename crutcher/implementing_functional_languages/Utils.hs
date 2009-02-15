{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Utils where

import Data.List as L
import Data.Map as M

{-
from: Implementing Functional Languages: a tutorial
by: Simon Peyton-Jons, David Lester

hInitial :: Heap a
hAlloc   :: Heap a -> a -> (Heap a, Addr)
hUpdate  :: Heap a -> Addr -> a -> Heap a
hFree    :: Heap a -> Addr -> Heap a

type Heap a = (Int, [Int], [(Int, a)])
type Addr = Int

hInitial                             = (0,      [1..],  [])
hAlloc  (size, (next:free), cts) n   = ((size+1, free,   (next,n) : cts),next)
hUpdate (size, free,        cts) a n = (size,   free,   (a,n) : remove cts a)
hFree   (size, free,        cts) a   = (size-1, a:free, remove cts a)
-}

newtype Addr = Addr Int
  deriving (Enum,Eq,Ord,Show)

class Heap h where
  hInitial :: h a
  hAlloc   :: a -> h a -> (h a, Addr)

  -- These three could fail, use Maybe or Error?
  hUpdate  :: Addr -> a -> h a -> h a
  hFree    :: Addr -> h a -> h a
  hLookup  :: Addr -> h a -> a

  hAddresses :: h a -> [Addr]
  hSize    :: h a -> Int


data PJLHeap a = PJLHeap !Int [Addr] ![(Addr, a)]

instance Heap PJLHeap where
  hInitial = PJLHeap 0 [Addr 1..] []
  hAlloc x (PJLHeap size (next:free) cts) = ((PJLHeap (size+1) free ((next,x) : cts)),next)
  hUpdate a x (PJLHeap size free cts) = PJLHeap size free ((a,x) : remove a cts)
  hFree a (PJLHeap size free cts) = PJLHeap (size-1) (a:free) (remove a cts)
  hLookup a (PJLHeap _ _ cts)
    = maybe (error ("can't find node " ++ show a ++ " in heap")) id
        (L.lookup a cts)
  hAddresses (PJLHeap _ _ cts)  = L.map fst cts
  hSize (PJLHeap s _ _)  = s

remove :: Addr -> [(Addr,a)] -> [(Addr,a)]
remove a []
  = error ("Attempt to update or free nonexistent address #" ++ show a)
remove a (h@(a',_):ls)
  | a == a'   = ls
  | otherwise = h : remove a ls



data MapHeap a = MapHeap [Addr] (Map Addr a)

instance Heap MapHeap where
  hInitial = MapHeap [Addr 1..] empty
  hAlloc x (MapHeap (next:free) m) = ((MapHeap free (M.insert next x m)), next)
  hUpdate a x (MapHeap free m) = MapHeap free (M.insert a x m)
  hFree a (MapHeap free m) = MapHeap (a:free) (M.delete a m)
  hLookup a (MapHeap _ m)
    = maybe (error ("can't find node " ++ show a ++ " in heap")) id
        (M.lookup a m)
  hAddresses (MapHeap _ m) = keys m
  hSize (MapHeap _ m) = size m

