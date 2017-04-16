module Main where

import System.Environment
import MpvLL
import Control.Monad.State
import Data.List.Split (splitOn)

data Track = Track { subId :: Maybe Int, speed :: Float, leadSecs :: Float, tailSecs :: Float }
     deriving (Show)



--[subtitleid/none[:speed[:lead_secs[:tail_secs]]]](repeatable) -- (mpv args)
readTrack :: String -> Maybe Track
readTrack str =
  do
    (track, _) <- runStateT rt2 (splitOn ":" str) -- returns Maybe (Track, [String])
    return track
  where
    rt2 :: StateT [String] Maybe Track
    rt2 = do
             subId <- popRead readJustOrNoneOrNothing Nothing
             speed <- popRead readOrNothing $ Just 1.0
             leadSecs <- popRead readOrNothing $ Just 0.0
             tailSecs <- popRead readOrNothing $ Just 0.0
             return $ Track subId speed leadSecs tailSecs

--reads either "none" which means not specified, or
-- a value of the corresponding type wrapped in a maybe
-- Returns:
--   Nothing - invalid input
--   Just Nothing - "none"
--   Just (Just value) - value
readJustOrNoneOrNothing :: Read a => (String -> Maybe (Maybe a))
readJustOrNoneOrNothing "none" = Just Nothing
readJustOrNoneOrNothing s = (readOrNothing s) >>= (\v -> Just $ Just v)

popRead :: Read a => (String -> Maybe a) -> (Maybe a) -> StateT [String] Maybe a
popRead reader def =
      StateT (doit reader def)
  where 
    doit :: Read a => (String -> Maybe a) -> Maybe a -> ([String] -> Maybe (a, [String]))
    doit reader def = ( \x -> case x of
                      [] -> (def >>= (\v -> Just (v, [])))
                      (s : ss) -> (reader s)
                                   >>= (\v -> Just (v, ss))
           )
           
readOrNothing :: Read a => String -> Maybe a
readOrNothing x =
      case (reads x) of
        [(v, [])] -> Just v
        _ -> Nothing
        
parseTracks :: [String] -> Either String [Track]
parseTracks [] = Right []
parseTracks (x : xs) =
  do
    t <- (case (readTrack x) of
              Nothing -> Left $ "Error, can't parse: "++x
              Just t -> Right t)
    ts <- parseTracks xs
    Right (t:ts)
                          

data Conf = Conf { tracks :: [Track], mpvArgs :: [String] } deriving (Show)

parseArgs :: [String] -> Either String Conf
parseArgs args = do
  tracks <- parseTracks (takeWhile (not . (== "--")) args)
  mpvArgs <- (case (dropWhile (not . (== "--")) args) of
    [] -> Left "args must contain '--'. Args after '--' are directly processed by mpv"
    (x : xs) -> Right xs)
  return $ Conf tracks mpvArgs


runit = runit

main :: IO ()
main =
  do
    args <- getArgs
    case (parseArgs args) of
      Right (c)-> runit c
      Left s -> putStrLn $ "Error: " ++ s
      
