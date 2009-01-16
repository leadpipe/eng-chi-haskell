
-- import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Char8 as C
import Data.Maybe
import System.IO

main = do
  ts <- C.getContents >>= return . splitEvery 3 . C.lines
  putStr $ unlines $ map nhay ts
  where
    nhay (k:ns:hs:[]) = case C.findSubstring ns hs of
      Nothing -> ""
      (Just x) -> show x

splitEvery n [] = []
splitEvery n xs = as : splitEvery n bs
  where (as,bs) = splitAt n xs
