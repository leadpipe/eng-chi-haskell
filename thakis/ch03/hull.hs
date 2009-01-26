

data Direction = DLeft | DRight | DStraight

dir :: (Double, Double) -> (Double, Double) -> (Double, Double) -> Direction
dir (x0, y0) (x1, y1) (x2, y2) =
  let (dx1, dy1) = (x1 - x0, y1 - y0)
      (dx2, dy2) = (x2 - x0, y2 - y0)
      cross = dx1*dy2 - dx2*dy1
      r | cross > 0 = DLeft
        | cross < 0 = DRight
        | otherwise = DStraight
  in r

