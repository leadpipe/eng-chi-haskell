
-- Write a program that transposes the text in a file. For instance,
-- it should convert "hello\nworld\n" to "hw\neo\nlr\nll\nod\n".

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
        myFunction = transposeText


peelHead :: [[Char]] -> ([Char], [[Char]])
peelHead [] = ("", [])
peelHead strlist = (map head strlist, map (drop 1) strlist)

buildLines :: [String] -> [String]
buildLines [] = []
buildLines [""] = []
buildlines somelines = peeledword : buildLines shorterlines
  where (peeledword, shorterlines) = peelHead somelines

transposeText :: String -> String
transposeText str = unlines (buildLines (lines str))

-- ## this doesn't quite work yet, not sure why.
