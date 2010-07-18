{-# LANGUAGE MultiParamTypeClasses, ScopedTypeVariables, TypeSynonymInstances #-}
-- | Defines the basic 3x3 cube puzzle.
module Rubik.Cube4 where

import Rubik.Algebra
import Rubik.Cycles
import Rubik.Cube
import Rubik.Geometry
import Rubik.Puzzle

import Control.Monad (sequence)
import Data.Array.IArray ((!), Array, listArray)
import Data.Char (toLower)
import Data.Ix (Ix)
import Data.List (elemIndex, transpose)
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
  fromEnum (FacePiece i) = i
  enumFrom = boundedEnumFrom
  enumFromThen = boundedEnumFromThen

instance Bounded FacePiece where
  minBound = FacePiece 0
  maxBound = FacePiece $ 3 + 4 * fromEnum (maxBound :: Face)

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
       fs <- sequence $ map (nameToMaybeFace . toLower) [c1, c2, c3]
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
  maxBound = EdgePiece $ 1 + 2 * fromEnum (maxBound :: Edge)

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
       fs <- sequence $ map (nameToMaybeFace . toLower) [c1, c2, c3]
       ep <- facesToMaybeEdgePiece fs
       return (ep, cs)
  readsPrec _ _ = []


instance Puzzle Cube4 Face where
  fromFaceTwist f 0 = Cube4 vw ew fw
    where vw = fromCycles [asCycle' f faceVertices vertexFaces]
          ew = fromCycles $ map edgeCycle edgePieces
          fw = fromCycles [asSimpleCycle $ faceFacePieces f]
          edgeCycle eps = asCycle f eps edgePieceFaces
          edgePieces = transpose $ map (map facesToEdgePiece)
                       [[[f, f2, f3], [f, f3, f2]] | [_, f2, f3] <- faceVerticesAsFaces f]
  fromFaceTwist f 1 = Cube4 one ew fw
    where ew = fromCycles [asCycle f edgePieces edgePieceFaces]
          fw = fromCycles $ map asSimpleCycle facePieces
          edgePieces = map facesToEdgePiece
                       [[f2, f3, f] | [_, f2, f3] <- faceVerticesAsFaces f]
          facePieces = transpose $ map (map facesToFacePiece)
                       [[[f2, f3, f], [f3, f2, f]] | [_, f2, f3] <- faceVerticesAsFaces f]


instance Show Cube4 where
  showsPrec n c@(Cube4 v e f) =
    if c == one then showEmptyParens else showVertices . showEdges . showFaces
      where showVertices = showNonemptyCycles Vertex v
            showEdges = showNonemptyCycles EdgePiece e
            showFaces = showNonemptyCycles FacePiece f
