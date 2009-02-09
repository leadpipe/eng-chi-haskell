
-- Using the command framework from the section called "A simple
-- command line framework", write a program that prints the first word
-- of each line of its input.

import System.Environment (getArgs)

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
  where mainWith function = do
          args <- getArgs
          case args of
            [input,output] -> interactWith function input output
            _ -> putStrLn "error: exactly two arguments needed"

        -- replace "id" with the name of our function below
        myFunction = printFirstWords


printFirstWords :: String -> String
printFirstWords [] = []
printFirstWords xs = unwords (map head (map words (filter (not . null) (lines xs))))
