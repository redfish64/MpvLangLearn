module SrtFile where
import Text.ParserCombinators.ReadP
import Control.Monad.State hiding (get)
import Control.Monad.Writer
import qualified Control.Monad.State as St
import GHC.Unicode (isSpace,isDigit)
import Control.Applicative ((<|>))
import qualified System.IO.Strict as SIS
import Control.DeepSeq (($!!),deepseq)
import GHC.IO.Encoding
import System.IO
import qualified Data.ByteString.Lazy as BS
    
data Srt = Srt { startTime :: Double, endTime :: Double, text :: [String], lines :: Int }
     deriving (Show)
data SrtError = SrtError { etext :: [String], elines :: Int }
     deriving (Show)


-- 1
-- 00:00:50,630 --> 00:00:52,564
-- [ Button Clicks ]

-- 2
-- 00:00:52,632 --> 00:00:55,931
-- [ Man On Tape ] Les, I'll tape a memo for you
-- on the Bronson matter tomorrow.

-- 3
-- 00:00:56,002 --> 00:01:00,268
-- As far as that Delthorne Corporation suit
-- is concerned, they've got me.

digits :: ReadP String
digits = munch1 isDigit

int :: ReadP Int
int = do
        n <- digits
        return (read n)

integer :: ReadP Integer
integer = do
        n <- digits
        return (read n)

--whitespace excluding newlines
whitespace1 :: ReadP String
whitespace1 = munch1 (\c -> isSpace c && (c /= '\n'))

--whitespace excluding newlines
whitespace :: ReadP String
whitespace = munch (\c -> isSpace c && (c /= '\n'))

--newline
line :: ReadP Char
line = satisfy (=='\n')

readLine :: StateT Int ReadP Char
readLine = do
  c <- lift line
  modify (+1)
  return c -- always will be '\n'

--reads zero or more lines containing the given pattern  
readLines :: (ReadP a) -> StateT Int ReadP [a]
readLines pat =
  do
    vals <- lift $ many
                (do
                  v <- pat
                  line
                  return v)
    modify (+ (length vals))
    return vals

--reads one or more lines containing the given pattern  
readLines1 :: (ReadP a) -> StateT Int ReadP [a]
readLines1 pat =
  do
    vals <- lift $ many1
                (do
                  v <- pat
                  line
                  return v)
    modify (+ (length vals))
    return vals

-- 00:00:50,630
srtTime :: ReadP Double
srtTime = do
             h <- int
             string ":"
             m <- int
             string ":"
             s <- int
             string ","
             ms <- int
             return $ (fromIntegral ms) * 0.001
                      + ((fromIntegral s)
                      + 60.0 * ((fromIntegral m)
                      + 60.0 * (fromIntegral h)))

-- a line containing a non whitespace character
nonEmpty :: ReadP String          
nonEmpty = do
                x1 <- many $ satisfy (\c -> (c /= '\n') && (isSpace c))
                c <- satisfy (\c -> (c /= '\n') && not (isSpace c))
                x2 <- many $ satisfy (\c -> (c /= '\n'))
                return $ x1 ++ [c] ++ x2

--skips some empty lines, at least one non-empty line, and one more empty line
skipSrt :: ReadP [String]
skipSrt =
  do
    (s, c) <- runStateT skipSrt2 0
    return s
  where
    skipSrt2 = do
      pre <- readLines whitespace
      sub <- readLines1 $ nonEmpty
      post <- lift $ whitespace
      readLine 
      return $ pre ++ sub ++ [post++"\n"]

srt :: ReadP Srt
srt = do
          (s, c) <- runStateT srt2 0
          return s
     where
       srt2 :: StateT Int ReadP Srt
       srt2 = do
         readLines whitespace
         -- 1
         index <- lift int
         lift whitespace
         readLine 
         -- 00:00:50,630 --> 00:00:52,564
         startTime <- lift $ srtTime
         lift $
           do
             whitespace
             string "-->" 
             whitespace
         endTime <- lift $ srtTime
         lift whitespace
         readLine
         -- <srt text> (multi line garbage)
         text <- readLines1 $ nonEmpty
         --empty lines at the end
         readLines1 whitespace
         lines <- St.get
         return $ Srt startTime endTime text lines

srtFile :: ReadP [ Srt ]
srtFile = do
            srts <- many srt
            eof
            return srts


--loads both utf16 and utf8 files
loadFile :: String -> IO String
loadFile f =
    do
      contents <- BS.readFile f
      case (BS.unpack contents) of
        --if a utf16 file
        (255 : 254 : s) -> loadFile2 utf16 f
        _ -> loadFile2 utf8 f
  where
    loadFile2 :: TextEncoding -> String -> IO String
    loadFile2 te f = 
        do
          handle <- openFile f ReadMode
          hSetEncoding handle te
          contents <- hGetContents handle
          -- strictly read the entire file, then close the handle
          contents `deepseq` hClose handle
          return contents
                 
                 
             


--loads an srt file.
--returns a list of srts and a list of strings indicating errors reading srts, if any
loadSrtFile :: String -> IO [Either [String] Srt]
loadSrtFile f =
  do
    contents <- loadFile f
    srts <- return $ loadSrts contents
    return srts
  where
    loadSrts :: String -> [Either [String] Srt]
    loadSrts [] = []
    loadSrts contents =
             case (readP_to_S srt contents) of
               [(srt,contents)] -> (Right srt) : loadSrts contents
               [] ->
                 case (readP_to_S skipSrt contents) of
                   [(skipSrt, contents)] -> Left skipSrt : loadSrts contents
                   [] -> [] --if we can't read skipSrt, there are no non blank lines left in
                            --the file
-- ts :: String -> IO ()
-- ts f =
--   do
--     handle <- openFile f ReadMode
--     contents <- hGetContents handle
--     contents `deepseq` hClose handle
--     srts <- return $! (readP_to_S srtFile contents)
--     hClose handle
--     putStrLn (show srts)
--     return ()


      
testSrt = "1\r\n00:00:50,630 --> 00:00:52,564\r\n[ Button Clicks ]\r\n\r\n"

  
