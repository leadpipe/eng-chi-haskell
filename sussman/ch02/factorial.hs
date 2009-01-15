-- Mandatory functional programming exercise.  :-)

factorial a = if (a == 1)
              then 1
              else (a * factorial (a-1))
