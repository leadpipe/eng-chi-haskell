import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Control.Applicative ((<$>))
import Data.Char (isSpace)
import Data.Int (Int64)
import Data.Word (Word8)

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


------------------------------------------------------------------------------
-- Second version: extract Maybe handling ------------------------------------
------------------------------------------------------------------------------

-- The "Maybe" wrapper
(>>?) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing >>? _ = Nothing
Just v >>? f = f v

-- Helper
skipSpace :: (a, L.ByteString) -> Maybe (a, L.ByteString)
skipSpace (a, s) = Just (a, L8.dropWhile isSpace s)


-- below, the code
-- a >>?
-- b >>?
-- c
--
-- is parsed as
--
-- ((a >>? b) >>? c).

parseP5_take2 s =
  matchHeader (L8.pack "P5") s >>?
  \s -> skipSpace ((), s) >>?
  (getNat . snd) >>?
  skipSpace >>?
  \(width, s) -> getNat s >>?
  skipSpace >>?
  \(height, s) -> getNat s >>?
  \(maxGrey, s) -> getBytes 1 s >>?
  (getBytes (width * height) . snd) >>?
  \(bitmapData, s) -> Just (Greymap width height maxGrey bitmapData, s)


------------------------------------------------------------------------------
-- Third version: Hiding the parse state -------------------------------------
------------------------------------------------------------------------------

-- Instead of returning `Maybe (some, pair)` as parse state from our parsing
-- functions, we return an algebraic data type instead. This makes it easier
-- to add more fields to the state because the parsing functions don't have to
-- do pattern matching to get access to the parse state (and hence, changing
-- the parse state does no longer break all parsing functions).

data ParseState = ParseState {
      string :: L.ByteString
    , offset :: Int64
    } deriving (Show)

-- A parse function takes a ParseState and produces a piece of data and a new
-- ParseState. It can also fail, in which case it returns an error message
-- (through Either).
newtype Parse a = Parse {
      runParse :: ParseState -> Either String (a, ParseState)
    }

-- Returns a parse function that returns the given value `val`
identity :: a -> Parse a
identity val = Parse (\s -> Right (val, s))

-- A parser that reads a single byte
-- (XXX: this uses a quite convoluted approach?)
parseByte :: Parse Word8
parseByte =
    getState ==> \initState ->
    case L.uncons (string initState) of
      Nothing ->
          bail "no more input"
      Just (byte,remainder) ->
          putState newState ==> \_ ->
          identity byte
        where newState = initState { string = remainder,
                                     offset = newOffset }
              newOffset = offset initState + 1

getState :: Parse ParseState
getState = Parse (\s -> Right (s, s))

putState :: ParseState -> Parse ()
putState s = Parse (\_ -> Right ((), s))
              
bail :: String -> Parse a
bail err = Parse $ \s -> Left $
           "byte offset " ++ show (offset s) ++ ": " ++ err
  
-- Chain two parsers: Given a parser that extracts an `a` and a function that
-- maps `a` to a parser that returns `b`, this returns a parser that returns
-- a `b`. Effectively, executes the first parser and feeds the result in the
-- function.
(==>) :: Parse a -> (a -> Parse b) -> Parse b
firstParser ==> secondParser  =  Parse chainedParser
  where chainedParser initState   =  -- note that we omit the state param here!
          case runParse firstParser initState of
            Left errMessage ->
                Left errMessage
            Right (firstResult, newState) ->
                runParse (secondParser firstResult) newState

-- Applies a regular function to a value returned by a parser
instance Functor Parse where
    fmap f parser = parser ==> \result ->
                    identity (f result)

peekByte :: Parse (Maybe Word8)
peekByte = (fmap fst . L.uncons . string) <$> getState

parseWhile :: (Word8 -> Bool) -> Parse [Word8]
parseWhile p = (fmap p <$> peekByte) ==> \mp ->
               if mp == Just True
               then parseByte ==> \b ->
                    (b:) <$> parseWhile p
               else identity []
  
                
------------------------------------------------------------------------------
-- Main function -------------------------------------------------------------
------------------------------------------------------------------------------

--parseP5 = parseP5_naive
parseP5 = parseP5_take2

main = do
  -- I created the pbm file like this:
  -- $ sudo port install netpbm
  -- $ tifftopnm path/to/32bit/image.tiff > test.ppm
  -- $ ppmtopgm test.ppm > test.pgm
  -- Alternatively, install imagemagick and do
  -- $ convert myfile.tiff test.pgm
  -- (For the tiff file, I opened some image in Preview.app and dragged its
  -- proxy icon in the title bar into Terminal.app)
  f <- L.readFile "test.pgm"
  let r = parseP5_naive f
  print r
