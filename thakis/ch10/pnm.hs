import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Char (isSpace)

data Greymap = Greymap {
      greyWidth :: Int
    , greyHeight :: Int
    , greyMax :: Int
    , greyData :: L.ByteString
    } deriving (Eq)

instance Show Greymap where
  show (Greymap w h m _) = "Greymap " ++ show w ++ "x" ++ show h
                           ++ " " ++ show m


-- All these functions try to read some data from a byte string and return
-- a (result, rest of bytestring) pair if they succeed.

parseP5 :: L.ByteString -> Maybe (Greymap, L.ByteString)

-- "nat" here is short for "natural number"
getNat :: L.ByteString -> Maybe (Int, L.ByteString)
getNat s = case L8.readInt s of
             Nothing -> Nothing
             Just (num,rest)
                 | num <= 0    -> Nothing
                 | otherwise -> Just (fromIntegral num, rest)

getBytes :: Int -> L.ByteString
         -> Maybe (L.ByteString, L.ByteString)
getBytes n str = let count           = fromIntegral n
                     both@(prefix,_) = L.splitAt count str
                 in if L.length prefix < count
                    then Nothing
                    else Just both

-- This function checks if a bytestring starts with a header and returns the
-- rest of the string if the match succeeds.
matchHeader :: L.ByteString -> L.ByteString -> Maybe L.ByteString
matchHeader prefix str
    | prefix `L8.isPrefixOf` str
        = Just (L8.dropWhile isSpace (L.drop (L.length prefix) str))
    | otherwise
        = Nothing

------------------------------------------------------------------------------
-- First version: Explicit error checking ------------------------------------
------------------------------------------------------------------------------

-- To debug, I changed the "Nothing" rules below with stuff like this, with
-- an error code in the greyMax field:
--     Nothing -> Just $ (Greymap 0 0 1 (L.pack []), L.pack [])
parseP5_naive s =
  case matchHeader (L8.pack "P5") s of
    --Nothing -> Just $ (Greymap 0 0 0 (L.pack []), L.pack [])
    Nothing -> Nothing
    Just s1 ->
      case getNat s1 of
        --Nothing -> Just $ (Greymap 0 0 1 (L.pack []), L.pack [])
        Nothing -> Nothing
        Just (width, s2) ->
          case getNat (L8.dropWhile isSpace s2) of
            --Nothing -> Just $ (Greymap 0 0 2 (L.pack []), L.pack [])
            Nothing -> Nothing
            Just (height, s3) ->
              case getNat (L8.dropWhile isSpace s3) of
                --Nothing -> Just $ (Greymap 0 0 3 (L.pack []), L.pack [])
                Nothing -> Nothing
                Just (maxGrey, s4)
                  | maxGrey > 255 -> Nothing
                  | otherwise ->
                      case getBytes 1 s4 of
                        Nothing -> Nothing
                        Just (_, s5) ->  -- skip newline
                          case getBytes (width * height) s5 of
                            Nothing -> Nothing
                            Just (bitmapData, s6) ->
                              Just (Greymap width height maxGrey bitmapData, s6)
                

parseP5 = parseP5_naive
main = do
  -- I created the pbm file like this:
  -- $ sudo port install netpbm
  -- $ tifftopnm path/to/32bit/image.tiff > test.ppm
  -- $ ppmtopgm test.ppm > test.pgm
  -- Alternatively, install imagemagick and do
  -- $ convert myfile.tiff test.pgm
  f <- L.readFile "test.pgm"
  let r = parseP5_naive f
  print r
