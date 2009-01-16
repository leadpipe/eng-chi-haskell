import qualified Data.ByteString.Lazy.Char8 as B
import Data.Maybe

main :: IO ()
main = do
  (nl:ts) <- B.getContents >>= return . B.lines
  let n = fst $ fromJust $ B.readInt nl
  putStr $ unlines $ map test $ take n ts
  where
    test :: B.ByteString -> String
    test line = case nsteps x y of
        Nothing -> "No Number"
        Just k  -> show k
      where [x,y] = map (fst . fromJust . B.readInt) $ B.words line

nsteps :: Int -> Int -> Maybe Int
nsteps x y
  | x == y     = Just (4 * div x 2 + (if even x then 0 else 1))
  | x == y + 2 = Just (4 * (div x 2) - (if even x then 2 else 1))
  | otherwise  = Nothing

