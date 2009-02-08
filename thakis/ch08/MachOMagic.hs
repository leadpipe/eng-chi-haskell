import qualified Data.ByteString.Lazy as L

-- See comment about Mach-O in /usr/share/file/magic
hasMachOMagic content =
     start == machOMagic
  || start == machOMagic64
  || start == machOMagicUniversal && content `L.index` 7 < 40
  where start = L.take 4 content
        machOMagic = L.pack [0xce, 0xfa, 0xed, 0xfe]
        machOMagic64 = L.pack [0xcf, 0xfa, 0xed, 0xfe]
        machOMagicUniversal = L.pack [0xca, 0xfe, 0xba, 0xbe]


isMachOFile f = L.readFile f >>= return . hasMachOMagic


-- Multi-architecture file (0xcafebabe)
name = "/Applications/Preview.app/Contents/MacOS/Preview"

-- Just one architecture (0xfeedface or 0xfeedfacf)
name2 = "/Users/nico/src/vim/MacVim/src/MacVim/build/Release/MacVim.app/Contents/MacOS/MacVim"

-- Class file (0xcafebabe as well)
name3 = "/Users/nico/src/javafun/NodeType.class"

main = do
  s <- L.readFile name
  putStrLn $ show $ hasMachOMagic s

  -- Is there an easier way to print the result of `isMachOFile` ?
  isMachOFile name >>= return . show >>= putStrLn

  isMachOFile name2 >>= return . show >>= putStrLn

  isMachOFile name3 >>= return . show >>= putStrLn
