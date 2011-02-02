{-# LANGUAGE TypeFamilies #-}
-- | Defines the basic 3x3 cube puzzle.
module Rubik.Cube4 where

import Rubik.Cycles
import Rubik.Cube
import Rubik.Group
import qualified Rubik.Memo as Memo
import Rubik.Polyhedron
import Rubik.Puzzle
import Rubik.Wreath
import Rubik.Twists

import Control.Monad (mapM)
import Data.Array.IArray ((!), Array, listArray)
import Data.Char (isUpper, toLower, toUpper)
import Data.Ix (Ix)
import Data.List (elemIndex, transpose)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, listToMaybe, maybeToList)
import Data.Monoid (Monoid, mappend, mempty)
import GHC.Enum (boundedEnumFrom, boundedEnumFromThen)

newtype Cube4 = Cube4 (Wreath Vertex, Wreath EdgePiece, Wreath FacePiece) deriving (Eq, Ord)

instance Monoid Cube4 where
  mempty = Cube4 one
  mappend (Cube4 s1) (Cube4 s2) = Cube4 (s1 $* s2)

instance Group Cube4 where
  ginvert (Cube4 s) = Cube4 $ ginvert s

-- | The face pieces, four for each of the cube's faces.
newtype FacePiece = FacePiece Int deriving (Eq, Ord, Ix)

-- | The edge pieces, two for each of the cube's edges.
newtype EdgePiece = EdgePiece Int deriving (Eq, Ord, Ix)

instance Enum FacePiece where
  toEnum = toBoundedEnum FacePiece
  fromEnum (FacePiece i) = i
  enumFrom = boundedEnumFrom
  enumFromThen = boundedEnumFromThen

instance Bounded FacePiece where
  minBound = FacePiece 0
  maxBound = FacePiece $ length allFacePiecesAsFaces - 1

instance WreathPermutable FacePiece where
  type WreathTwist FacePiece = Twistless

allFacePiecesAsFaces = concat [faceVerticesAsFaces f | f <- [minBound..]]

facePieceFaces :: FacePiece -> [Face]
facePieceFaces = (facesArray !) . fromEnum
  where facesArray = makeFacesArray allFacePiecesAsFaces

facesToMaybeFacePiece :: [Face] -> Maybe FacePiece
facesToMaybeFacePiece (f:fs) = (f, facesIndex fs) `Map.lookup` fpMap
  where fpMap :: Map (Face, Int) FacePiece
        fpMap = Map.fromList [((f, i), fp) | fp <- [minBound..],
                              let (f:fs) = facePieceFaces fp,
                              let i = facesIndex fs]

facesToFacePiece :: [Face] -> FacePiece
facesToFacePiece = fromJust . facesToMaybeFacePiece

facePieceFace :: FacePiece -> Face
facePieceFace = toEnum . (`div` 4) . fromEnum

faceFacePieces :: Face -> [FacePiece]
faceFacePieces = (piecesArray !)
  where piecesArray :: Array Face [FacePiece]
        piecesArray = listArray (minBound, maxBound)
                      [map facesToFacePiece $ faceVerticesAsFaces f | f <- [minBound..]]

instance Show FacePiece where
  showsPrec _ = showString . toString
    where toString fp = let (fn:fns) = map faceToName $ facePieceFaces fp
                        in fn:'.':fns

instance Read FacePiece where
  readsPrec _ (c1:dot:c2:c3:cs)
   | dot == '.' = maybeToList $ do
       fs <- mapM (nameToMaybeFace . toLower) [c1, c2, c3]
       fp <- facesToMaybeFacePiece fs
       return (fp, cs)
  readsPrec _ _ = []

instance Enum EdgePiece where
  toEnum = toBoundedEnum EdgePiece
  fromEnum (EdgePiece i) = i
  enumFrom = boundedEnumFrom
  enumFromThen = boundedEnumFromThen

instance Bounded EdgePiece where
  minBound = EdgePiece 0
  maxBound = EdgePiece $ length allEdgePiecesAsFaces - 1

instance WreathPermutable EdgePiece where
  type WreathTwist EdgePiece = Twistless

allEdgePiecesAsFaces = concat [pieces e | e <- [minBound..]]
  where pieces :: Edge -> [[Face]]
        pieces e = let [f, n] = edgeFaces e
                   in [[f, n, previousNeighbor f n], [f, n, nextNeighbor f n]]

edgePieceFaces :: EdgePiece -> [Face]
edgePieceFaces = (facesArray !) . fromEnum
  where facesArray = makeFacesArray allEdgePiecesAsFaces

facesToMaybeEdgePiece :: [Face] -> Maybe EdgePiece
facesToMaybeEdgePiece [f1, f2, f] = (facesIndex [f1, f2], f) `Map.lookup` epMap
  where epMap :: Map (Int, Face) EdgePiece
        epMap = Map.fromList [((i, f), ep) | ep <- [minBound..],
                              let [f1,f2,f] = edgePieceFaces ep,
                              let i = facesIndex [f1,f2]]

facesToEdgePiece :: [Face] -> EdgePiece
facesToEdgePiece = fromJust . facesToMaybeEdgePiece

edgePieceEdge :: EdgePiece -> Edge
edgePieceEdge = toEnum . (`div` 2) . fromEnum

instance Show EdgePiece where
  showsPrec _ = showString . toString
    where toString ep = let (c1:c2:cs) = map faceToName (edgePieceFaces ep)
                        in c1:c2:'.':cs

instance Read EdgePiece where
  readsPrec _ (c1:c2:dot:c3:cs)
   | dot == '.' = maybeToList $ do
       fs <- mapM (nameToMaybeFace . toLower) [c1, c2, c3]
       ep <- facesToMaybeEdgePiece fs
       return (ep, cs)
  readsPrec _ _ = []

-- | Possible moves for a 4x4 cube; the boolean means "outer layer only", so
-- when false means to twist both layers.
data FaceTwist4 = FT4 Face Bool Twist4 deriving (Eq, Ord, Bounded)

instance PuzzleMove FaceTwist4 where
  undoMove (FT4 f b t) = FT4 f b (-t)

  joinMoves m1@(FT4 f1 b1 t1) m2@(FT4 f2 b2 t2)
    | f1 == f2 && b1 == b2           = let t = t1 + t2 in
                                       if t == 0 then [] else [FT4 f1 b1 t]
    | f1 == f2 || f1 `isOpposite` f2 = [max m1 m2, min m1 m2]
    | otherwise                      = [m1, m2]

  isTrivialMove (FT4 _ _ t) = t == 0


instance Show FaceTwist4 where
  showsPrec _ (FT4 f b t) = (if b then shows else showChar.toUpper.faceToName) f . shows t

instance Read FaceTwist4 where
  readsPrec _ "" = []
  readsPrec _ (c:s) = maybeToList $ do
    f <- nameToMaybeFace (toLower c)
    (t, s') <- listToMaybe (reads s)
    return (FT4 f (not.isUpper$c) t, s')


instance Puzzle Cube4 where
  type Move Cube4 = FaceTwist4

  fromMove = table $ Memo.memo3 Memo.array Memo.bool Memo.array fromMove4
    where table m (FT4 f b t) = m f b t

fromMove4 :: Face -> Bool -> Twist4 -> Cube4
fromMove4 f True 1 = Cube4 (vw, ew, fw)
  where vw = fromCycles [asCycle' f faceVertices vertexFaces]
        ew = fromCycles $ map edgeCycle edgePieces
        fw = fromCycles [asSimpleCycle $ faceFacePieces f]
        edgeCycle eps = asCycle f eps edgePieceFaces
        edgePieces = transpose $ map (map facesToEdgePiece)
                     [[[f, f2, f3], [f, f3, f2]] | [_, f2, f3] <- faceVerticesAsFaces f]

fromMove4 f False 1 = Cube4 (one, ew, fw) $* fromMove (FT4 f True 1)
  where ew = fromCycles [asCycle f edgePieces edgePieceFaces]
        fw = fromCycles $ map asSimpleCycle facePieces
        edgePieces = map facesToEdgePiece
                     [[f2, f3, f] | [_, f2, f3] <- faceVerticesAsFaces f]
        facePieces = transpose $ map (map facesToFacePiece)
                     [[[f2, f3, f], [f3, f2, f]] | [_, f2, f3] <- faceVerticesAsFaces f]

fromMove4 f b n = fromMove (FT4 f b 1) $^ n


instance Show Cube4 where
  showsPrec _ (Cube4 (v, e, f)) = fromOptCycles $ optShowCycles v $* optShowCycles e $* optShowCycles f

c4 :: String -> Algorithm Cube4
c4 = read