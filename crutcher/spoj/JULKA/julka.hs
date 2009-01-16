import qualified Data.ByteString.Lazy.Char8 as B
import Data.Maybe

main :: IO ()
main = do
  ts <- B.getContents >>= return . map (fst . fromJust . B.readInteger) . B.lines
  runTests ts
  where runTests :: [Integer] -> IO ()
        runTests (t:d:ts) = test t d >> runTests ts
        runTests _ = return ()

test total diff = print klaudia >> print natalia
  where
  natalia = div (total - diff) 2
  klaudia = natalia + diff

