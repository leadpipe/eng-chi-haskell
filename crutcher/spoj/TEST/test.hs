import Control.Monad

sequenceWhile :: Monad m => (a -> Bool) -> [m a] -> m [a]
sequenceWhile f [] = return []
sequenceWhile f (x:xs)
  = do v <- x
       if f v
         then sequenceWhile f xs >>= return . (v:)
         else return []

main' = do xs <- (sequenceWhile (/=42) $ repeat (getLine >>= return . read))
           sequence $ map print xs


