module SrtFile where
import Text.ParserCombinators.ReadP
import Control.Monad.State hiding (get)
import qualified Control.Monad.State as St
import GHC.Unicode (isSpace)
import Control.Applicative ((<|>))
import System.IO

data Srt = Srt { startTime :: Int, endTime :: Int, text :: [String], lines :: Int }
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
digits = munch1 (\c -> (c >= '0' && c <='9'))

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
srtTime :: ReadP Int
srtTime = do
             h <- int
             string ":"
             m <- int
             string ":"
             s <- int
             string ","
             ms <- int
             return $ ms + 1000 * (s + 60 * (m + 60 * h))

-- a line containing a non whitespace character
nonEmpty :: ReadP String          
nonEmpty = do
                x1 <- many $ satisfy (\c -> (c /= '\n') && (isSpace c))
                c <- satisfy (\c -> (c /= '\n') && not (isSpace c))
                x2 <- many $ satisfy (\c -> (c /= '\n'))
                return $ x1 ++ [c] ++ x2

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

srtError :: ReadP SrtError
srtError = do
          (s, c) <- runStateT srtError2 0
          return s
      where
       srtError2 :: StateT Int ReadP SrtError
       srtError2 = do
         --if we don't understand it, just skip all the crap until we get to another blank line
         readLines whitespace
         text <- readLines1 nonEmpty
         readLines whitespace
         lines <- St.get
         return $ SrtError text lines

srtFile :: ReadP [ Srt ]
srtFile = do
            srts <- many srt
            eof
            return srts
            

--TODO handle errors
loadSrtFile :: String -> IO [ Srt ]
loadSrtFile f =
  do
    handle <- openBinaryFile f ReadMode
    contents <- hGetContents handle
    [(srts, _)] <- return $! (readP_to_S srtFile contents)
    hClose handle
    return srts

         
testSrt = "1\r\n00:00:50,630 --> 00:00:52,564\r\n[ Button Clicks ]\r\n\r\n"

  
