-- Other mandatory functional programming exercise.

penultimate xs = last (take ((length xs) - 1) xs)

fibonacci a = if (a == 1) || (a == 2)
              then 1
              else (fibonacci (a-1)) + (fibonacci (a-2))

fiblist n = if (n == 0)
            then []
            else (fiblist (n - 1)) ++ [fibonacci n]

-- a different version, which doesn't call fibonacci

-- we could make it faster if it only computed fiblist2 (n-1) *once*
-- instead of 3 times, right?

fiblist2 n = if (n == 0)
             then []
             else if (n == 1)
                  then [1]
                  else if (n == 2)
                       then [1, 1]
                       else (fiblist2 (n - 1)) ++ [last (fiblist2 (n - 1)) + penultimate (fiblist2 (n - 1))]
