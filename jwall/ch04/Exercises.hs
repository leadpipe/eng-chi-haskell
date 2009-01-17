--file: ch04/Exercises.hs

data Item a = Item a
            | Undefined
            deriving (Show)

--safeHead

safeHead [] = Undefined
safeHead (x:xs) = Item x

--safeTail

safeTail [] = []
safeTail (x:xs) = xs

--safeLast

safeLast [] = Undefined
safeLast (x:[]) = Item x
safeLast (x:xs) = safeLast(xs)

--safeInit

safeInit [] = [Undefined]
safeInit (x1:x2:[]) = [Item x1]
safeInit (x:xs) = Item x:safeInit(xs)

