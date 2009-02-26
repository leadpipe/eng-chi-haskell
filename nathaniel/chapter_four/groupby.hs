-- Nathaniel's implementation of groupBy. Ben and I did this at
--   the whiteboard when no one else showed up to the Chapter Four
--   meeting.
groupBy f = foldr step []
  where step e [] = [[e]]
        step e (candidates:closed)
            | f e (head candidates) = (e:candidates):closed
            | otherwise             = [e]:(candidates:closed)
