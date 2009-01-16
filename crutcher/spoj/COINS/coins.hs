import Control.Monad
import Data.Array.ST
import Data.Array.Unboxed

-- This overflows for large values of n
coinArray :: Int -> UArray Int Int
coinArray n = runSTUArray (do
  arr <- newArray_ (0,n)
  writeArray arr 0 0
  forM_ [1..n] (\i -> do
    v2 <- readArray arr (div i 2)
    v3 <- readArray arr (div i 3)
    v4 <- readArray arr (div i 4)
    writeArray arr i (max i (v2 + v3 + v4)))
  return arr)

coinValueA :: Integral a => UArray Int Int -> Int -> a
coinValueA arr n = cv n
  where
  (_,b) = bounds arr
  cv x | x <= b = fromIntegral (arr ! x)
       | otherwise = max (fromIntegral x) $ sum $ map (cv . div x) [2,3,4]

coinValue :: Int -> Integer
coinValue = coinValueA coins
  where coins = coinArray 250000
  -- 250000 determined experimentally

main = interact g
  where g = unlines . map (show . coinValue . read) . lines

