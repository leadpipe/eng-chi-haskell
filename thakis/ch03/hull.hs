import Data.List (sortBy)
import Data.Function (on)

data Direction = DLeft | DRight | DStraight
     deriving (Show)

dir :: (Double, Double) -> (Double, Double) -> (Double, Double) -> Direction
dir (x0, y0) (x1, y1) (x2, y2) =
  let (dx1, dy1) = (x1 - x0, y1 - y0)
      (dx2, dy2) = (x2 - x0, y2 - y0)
      cross = dx1*dy2 - dx2*dy1
      -- XXX: this doesn't deal very well with floating point inaccuracies
      r | cross > 0 = DLeft
        | cross < 0 = DRight
        | otherwise = DStraight
  in r


allDirs :: [(Double, Double)] -> [Direction]
allDirs (p0:p1:p2:ps) = dir p0 p1 p2 : allDirs (p1:p2:ps)
allDirs _ = []


scanHull :: [(Double, Double)] -> [(Double, Double)]
scanHull l = undefined  -- XXX
  where start@(sx, sy) = minimum l
        key (x, y) = (y - sy) / (x - sx)  -- XXX: div 0 
        lp0:lp1:lps = sortBy (compare `on` key) (delete l start)
        f [] _ _ cands = cands
        f (p:ps) p1 p2 cands = if (dir p1 p2 p) == DLeft


