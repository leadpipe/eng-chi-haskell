
-- Return the penultimate member of a list, or () if not available.

penultimate xs = last (take ((length xs) - 1) xs)
