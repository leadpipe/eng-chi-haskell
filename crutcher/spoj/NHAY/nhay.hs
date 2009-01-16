
import Data.List

nhay :: String -> String -> [Int]
nhay ns = findIndices (isPrefixOf ns) . tails

main = interact (unlines . f . lines)
  where f :: [String] -> [String]
        f (_:ns:hs:lss) = oss ++ f lss
          where ps = nhay ns hs
                oss = case ps of
                  [] -> [""]
                  _ -> map show ps
        f _ = []

