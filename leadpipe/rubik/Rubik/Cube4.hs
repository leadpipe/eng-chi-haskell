{-# LANGUAGE MultiParamTypeClasses, ScopedTypeVariables, TypeSynonymInstances #-}
-- | Defines the basic 3x3 cube puzzle.
module Rubik.Cube4 where

import Rubik.Algebra
import Rubik.Cycles
import Rubik.Cube
import Rubik.Geometry
import Rubik.Puzzle

import Data.Array.IArray ((!))
import Data.Char (toLower)
import Data.Ix (Ix)
import Data.List (elemIndex)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, maybeToList)
import Data.Monoid (Monoid, mappend, mempty)
import GHC.Enum (boundedEnumFrom, boundedEnumFromThen)

data Cube4 = Cube4 VertexWreath EdgeWreath FaceWreath deriving (Eq, Ord)

instance Monoid Cube4 where
  mempty = Cube4 one one one
  mappend (Cube4 v1 e1 f1) (Cube4 v2 e2 f2) = Cube4 (v1 *> v2) (e1 *> e2) (f1 *> f2)

-- | The face pieces, four for each of the cube's faces.
newtype FacePiece = FacePiece Int deriving (Eq, Ord, Ix)

-- | The edge pieces, two for each of the cube's edges.
newtype EdgePiece = EdgePiece Int deriving (Eq, Ord, Ix)

instance Enum FacePiece where
  toEnum = toBoundedEnum FacePiece
  fromEnum (FacePiece e) = e
  enumFrom = boundedEnumFrom
  enumFromThen = boundedEnumFromThen

instance Bounded FacePiece where
  minBound = FacePiece 0
  maxBound = FacePiece $ 4 * fromEnum (maxBound :: Face)

allFacePiecesAsFaces = concat [faceVerticesAsFaces f | f <- [minBound..]]

facePieceFaces :: FacePiece -> [Face]
facePieceFaces = (facesArray !) . fromEnum
  where facesArray = makeFacesArray allFacePiecesAsFaces

facesToMaybeFacePiece :: Face -> [Face] -> Maybe FacePiece
facesToMaybeFacePiece f fs = (f, facesIndex fs) `Map.lookup` fpMap
  where fpMap :: Map (Face, Int) FacePiece
        fpMap = Map.fromList [((f, i), fp) | fp <- [minBound..],
                              let (f:fs) = facePieceFaces fp,
                              let i = facesIndex fs]

instance Show FacePiece where
  showsPrec _ = showString . toString
    where toString fp = let (fn:fns) = map faceToName (facePieceFaces fp)
                        in fn:'.':fns

instance Read FacePiece where
  readsPrec _ (c1:dot:c2:c3:cs)
   | dot == '.' = maybeToList $ do
       f1 <- nameToMaybeFace (toLower c1)
       f2 <- nameToMaybeFace (toLower c2)
       f3 <- nameToMaybeFace (toLower c3)
       fp <- facesToMaybeFacePiece f1 [f2, f3]
       return (fp, cs)
  readsPrec _ _ = []

instance Enum EdgePiece where
  toEnum = toBoundedEnum EdgePiece
  fromEnum (EdgePiece e) = e
  enumFrom = boundedEnumFrom
  enumFromThen = boundedEnumFromThen
  
instance Bounded EdgePiece where
  minBound = EdgePiece 0
  maxBound = EdgePiece $ 2 * fromEnum (maxBound :: Edge)

allEdgePiecesAsFaces = concat [pieces e | e <- [minBound..]]
  where pieces :: Edge -> [[Face]]
        pieces e = let [f, n] = edgeFaces e
                   in [[f, n, previousNeighbor f n], [f, n, nextNeighbor f n]]

edgePieceFaces :: EdgePiece -> [Face]
edgePieceFaces = (facesArray !) . fromEnum
  where facesArray = makeFacesArray allEdgePiecesAsFaces

facesToMaybeEdgePiece :: [Face] -> Face -> Maybe EdgePiece
facesToMaybeEdgePiece fs f = (facesIndex fs, f) `Map.lookup` epMap
  where epMap :: Map (Int, Face) EdgePiece
        epMap = Map.fromList [((i, f), ep) | ep <- [minBound..],
                              let [f1,f2,f] = edgePieceFaces ep,
                              let i = facesIndex [f1,f2]]

instance Show EdgePiece where
  showsPrec _ = showString . toString
    where toString ep = let (c1:c2:cs) = map faceToName (edgePieceFaces ep)
                        in c1:c2:'.':cs

instance Read EdgePiece where
  readsPrec _ (c1:c2:dot:c3:cs)
   | dot == '.' = maybeToList $ do
       f1 <- nameToMaybeFace (toLower c1)
       f2 <- nameToMaybeFace (toLower c2)
       f3 <- nameToMaybeFace (toLower c3)
       ep <- facesToMaybeEdgePiece [f1, f2] f3
       return (ep, cs)
  readsPrec _ _ = []
{-
instance Puzzle Cube4 Face where
  fromFaceTwist f 0 = Cube4 vw ew fw
    where vw = fromCycles [asCycle faceVertices vertexFaces]
          ew = fromCycles [asCycle faceEdges edgeFaces]
          asCycle :: forall a t. (Enum a, Monoid t, Ord t, Num t) =>
                     (Face -> [a]) -> (a -> [Face]) -> [WreathMove t]
          asCycle toAs toFs = map toWM $ zip as $ rotate 1 as
            where as = toAs f
                  toWM (a1, a2) = WM (fromEnum a1) (twist (toFs a1) (toFs a2))
                  twist fs1 fs2 = fromInteger (indexIn fs2 - indexIn fs1)
                  indexIn fs = toInteger $ fromJust $ f `elemIndex` fs

instance Show Cube4 where
  showsPrec n (Cube4 v e f) =
    showCycles showVertexMove v . showChar '|' . showCycles showEdgeMove e
      where showVertexMove = showMove Vertex
            showEdgeMove = showMove Edge
            showMove fromInt (WM i t) = showsPrec 0 (fromInt i) . showsPrec 0 t
-}