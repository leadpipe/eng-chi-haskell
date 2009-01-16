-- Nathaniel's solutions for "Real World Haskell" Chapter 3 exercises.
--  Except for the mid-chapter exercises. Give me a break.

import Data.Ord
import Data.List

-- 1,2
_elementCount a (x:xs) = _elementCount (a + 1) xs
_elementCount a []   = a
elementCount l = _elementCount 0 l :: Int

-- 3
_mean (sum, count) (x:xs) = _mean (sum + x, count + 1) xs
_mean (sum, count) []     = sum / count
mean []     = error "An empty list has no mean value!"
mean l  = _mean (0, 0) l

-- 4
_reverse [] l = l
_reverse (x:xs) l = _reverse xs (x:l)
palin l = let reversed = _reverse l [] in _reverse reversed reversed

-- 5
isPalin l = let reversed = _reverse l [] in reversed == l

-- 6
sortByLength = sortBy (comparing length)

-- 7
_intersperse separator ll = foldr (\x y -> separator:(x ++ y)) [] ll
intersperse _ [] = []
intersperse separator (x:xs) = x ++ (_intersperse separator xs)

-- 8
data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving (Show)

height Empty               = 0
height (Node _ left right) = 1 + max (height left) (height right)

-- 9
data Direction = DirectionLeft | DirectionStraight | DirectionRight
  deriving (Show)

-- 10
data Point2D = Point2D {
  x :: Double
 ,y :: Double
} deriving (Show)

_vector (Point2D x1 y1) (Point2D x2 y2) = Point2D (x2 - x1) (y2 - y1)
turn a b c =
  let
    (Point2D x1 y1) = _vector a b
    (Point2D x2 y2) = _vector b c
    z               = x1 * y2 - x2 * y1
  in
    case (compare 0 z) of
      LT -> DirectionLeft
      EQ -> DirectionStraight
      GT -> DirectionRight

-- 11
_weirdFold f [x,y,z] = [f x y z]
_weirdFold f (x:y:z:more) = (f x y z):(_weirdFold f (y:z:more))
turns = _weirdFold turn

-- 12
__leftmostLowest candidate [] acc = (candidate,acc)
__leftmostLowest (Point2D cx cy) ((Point2D lx ly):more) acc =
  case (compare cy ly) of
    LT -> __leftmostLowest (Point2D cx cy) more ((Point2D lx ly):acc)
    EQ -> case (compare cx lx) of
            LT -> __leftmostLowest (Point2D cx cy) more ((Point2D lx ly):acc)
            EQ -> error "Point appears twice in list!"
            GT -> __leftmostLowest (Point2D lx ly) more ((Point2D cx cy):acc)
    GT -> __leftmostLowest (Point2D lx ly) more ((Point2D cx cy):acc)
_leftmostLowest (point:points) = __leftmostLowest point points []
_negativeXAxisCotangent (Point2D px py) (Point2D qx qy) = (px - qx) / (qy - py)
_convexHull [lastPoint] acc = lastPoint:acc
_convexHull (q:r:more) (hullPoint:restOfAcc) =
  case turn hullPoint q r of
    DirectionLeft -> _convexHull (r:more) (q:hullPoint:restOfAcc)
    _             -> _convexHull (r:more) (hullPoint:restOfAcc)
convexHull points =
  let
    (p, remainder) = _leftmostLowest points
    sortedRemainder = sortBy (comparing (_negativeXAxisCotangent p)) remainder
  in
    _convexHull sortedRemainder [p]
