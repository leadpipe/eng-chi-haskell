main = interact wordCount
  --where wordCount input = show (length (lines input)) ++ "\n"  -- lines
  --where wordCount input = show (length (words input)) ++ "\n"  -- words
  where wordCount input = show (length input) ++ "\n"  -- chars
