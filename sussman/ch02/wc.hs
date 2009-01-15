-- my first haskell program
-- this line is a comment.

main = interact wordCount
   where wordCount input = show (length input) ++ "\n"
