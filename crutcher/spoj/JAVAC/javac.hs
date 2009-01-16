
import Data.Maybe

isJavaIdentifier [] = False  
isJavaIdentifier (c:cs) = isAsciiLower c && all isIdChar cs
  where isIdChar c = isAscii c && isAlpha c

