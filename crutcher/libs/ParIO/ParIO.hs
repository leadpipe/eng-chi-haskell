-- | Provides parallel forms of 'forM' and 'mapM'.
-- From a conversation with Saizan on #haskell.
module ParIO 
  (parMapIO, parMapIO_, parMapIO__,
   parForIO, parForIO_, parForIO__)
where

import Test.HUnit.Base
import Test.HUnit.Text

import Control.Concurrent (forkIO,putMVar,newEmptyMVar,takeMVar,tryTakeMVar,threadDelay)
import Control.Monad (forM,forM_,when)
import Data.Maybe (fromJust)
import System.IO.Unsafe (unsafePerformIO)
import Control.Exception (evaluate)

-- | 'parMapIO' is a parallel version of 'mapM' in the 'IO' monad.
-- Each element is processed in its own forkIO thread, and the results are
-- collected and returned in the original order. This function is lazy in
-- the returned values.
parMapIO :: (a -> IO b) -> [a] -> IO [b]
parMapIO f xs = do
  ms <- forM xs $ \a -> do
    m <- newEmptyMVar
    forkIO (f a >>= putMVar m)
    return m
  mapM takeMVar ms

-- | 'parForIO' is 'parMapIO' with its arguments flipped,
-- this makes it a parallel form of 'forM' in the 'IO' monad.
parForIO :: [a] -> (a -> IO b) -> IO [b]
parForIO = flip parMapIO

-- | 'parMapIO\'' is a parallel version of 'mapM' in the 'IO' monad.
-- Each element is processed in its own forkIO thread, and the results are
-- collected and returned in the original order. This function is strict in
-- the returned values, they are evaluated before being returned.
parMapIO' :: (a -> IO b) -> [a] -> IO [b]
parMapIO' f = parMapIO (\x -> f x >>= evaluate)

-- | 'parForIO\'' is 'parMapIO\'' with its arguments flipped,
-- this makes it a parallel form of 'forM' in the 'IO' monad.
-- This function is strict in the returned values, they are evaluated
-- before being returned.
parForIO' :: [a] -> (a -> IO b) -> IO [b]
parForIO' = flip parMapIO'

-- | 'parMapIO_' is a parallel version of 'mapM_' in the 'IO' monad.
-- Each element is processed in its own forkIO thread, and all threads are
-- run to completion before 'parMapIO_' completes.
parMapIO_ :: (a -> IO b) -> [a] -> IO ()
parMapIO_ f xs = do
  ms <- forM xs $ \a -> do
    m <- newEmptyMVar
    forkIO (f a >>= putMVar m)
    return m
  mapM_ takeMVar ms

-- | 'parForIO_' is 'parMapIO_' with its arguments flipped,
-- this makes it a parallel form of 'forM_' in the 'IO' monad.
parForIO_ :: [a] -> (a -> IO b) -> IO ()
parForIO_ = flip parMapIO_

-- | 'parMapIO__' is a parallel version of 'mapM_' in the 'IO' monad.
-- Each element is processed in its own forkIO thread, and then
-- 'parMapIO__' returns immediately, without waiting for the threads
-- to complete.
parMapIO__ :: (a -> IO b) -> [a] -> IO ()
parMapIO__ f xs = do
  forM_ xs $ \a -> do
    forkIO (f a >> return ())

-- | 'parForIO__' is 'parMapIO__' with its arguments flipped,
-- this makes it a parallel form of 'forM_' in the 'IO' monad.
parForIO__ :: [a] -> (a -> IO b) -> IO ()
parForIO__ = flip parMapIO__



unsafePutMVar m v = unsafePerformIO (putMVar m v)

testParMapIO = commonTestParForIO (flip parMapIO)
testParForIO = commonTestParForIO parForIO

commonTestParForIO f = TestCase $ do
  magicMVar <- newEmptyMVar
  [v] <- f [()] $ \_ -> return (unsafePutMVar magicMVar ())
  tryTakeMVar magicMVar
    >>= assertBool "Result should not be available" . (== Nothing)
  evaluate v
  takeMVar magicMVar >>= (return . (== ()))
    >>= assertBool "Result should be ()" 


testParMapIO' = commonTestParForIO' (flip parMapIO')
testParForIO' = commonTestParForIO' parForIO'

commonTestParForIO' f = TestCase $ do
  magicMVar <- newEmptyMVar
  [v] <- f [()] $ \_ -> return (unsafePutMVar magicMVar ())
  takeMVar magicMVar >>= (return . (== ()))
    >>= assertBool "Result should be ()" 


testParMapIO_ = commonTestParForIO_ (flip parMapIO_)
testParForIO_ = commonTestParForIO_ parForIO_

commonTestParForIO_ f = TestCase $ do
  permitRun <- newEmptyMVar
  forReturned <- newEmptyMVar
  magicMVar <- newEmptyMVar
  forkIO $ do
    () <- f [()] $ \_ -> do
      takeMVar permitRun
      return (unsafePutMVar magicMVar ())
    putMVar forReturned ()
  threadDelay 1
  tryTakeMVar forReturned
    >>= assertBool "for/map should still be pending" . (== Nothing)
  putMVar permitRun ()
  takeMVar forReturned
  threadDelay 1
  tryTakeMVar magicMVar
    >>= assertBool "Result should not be available" . (== Nothing)


testParMapIO__ = commonTestParForIO__ (flip parMapIO__)
testParForIO__ = commonTestParForIO__ parForIO__

commonTestParForIO__ f = TestCase $ do
  permitRun <- newEmptyMVar
  forReturned <- newEmptyMVar
  magicMVar <- newEmptyMVar
  forkIO $ do
    () <- f [()] $ \_ -> do
      takeMVar permitRun
      return (unsafePutMVar magicMVar ())
    putMVar forReturned ()
  takeMVar forReturned
  putMVar permitRun ()
  threadDelay 1
  tryTakeMVar magicMVar
    >>= assertBool "Result should not be available" . (== Nothing)


tests = TestList [
  TestLabel "testParMapIO" testParMapIO,
  TestLabel "testParForIO" testParForIO,
  TestLabel "testParMapIO'" testParMapIO',
  TestLabel "testParForIO'" testParForIO',
  TestLabel "testParMapIO_" testParMapIO_,
  TestLabel "testParForIO_" testParForIO_,
  TestLabel "testParMapIO__" testParMapIO__,
  TestLabel "testParForIO__" testParForIO__]

main = runTestTT tests

