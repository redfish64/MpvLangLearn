module Init(
            Conf(..), MpvArgs(..), parseArgs, createAndSortLoopArrays) where

import System.Environment
import MpvFFI
import Control.Monad.State
import Data.List.Split (splitOn)
import Data.List (isPrefixOf,elemIndex)
import Text.Read (readMaybe)
import Control.Exception.Base (Exception,throwIO)
import SrtFile (loadSrtFile, Srt)
import qualified SrtFile as SF
import Loops
import Control.Monad.Trans.Either

import Foreign (Ptr)
import EventLoop (EventLoop,createEventLoop,Track(..))
  

type MpvFlag = String

type MpvOption = (String,String)

data MpvArgs = MpvArgs { flags :: [MpvFlag], opts :: [MpvOption], singleArgs :: [String] } deriving (Show)

data Conf = Conf { subfiles :: [String], tracks :: [Track], mpvArgs :: MpvArgs } deriving (Show)



--[subtitleid/none[:speed[:lead_secs[:tail_secs]]]](repeatable) -- (mpv args)
readTrack :: String -> Maybe Track
readTrack str =
  do
    (track, _) <- runStateT rt2 (splitOn ":" str) -- returns Maybe (Track, [String])
    return track
  where
    rt2 :: StateT [String] Maybe Track
    rt2 = do
             sids <- popRead readSids Nothing
             speed <- popRead readOrNothing $ Just 1.0
             leadSecs <- popRead readOrNothing $ Just 0.0
             tailSecs <- popRead readOrNothing $ Just 0.0
             return $ Track sids speed leadSecs tailSecs

joinMaybe :: [Maybe a] -> Maybe [a]
joinMaybe [] = Just []
joinMaybe (Nothing : _) = Nothing
joinMaybe ((Just x) : xs) = joinMaybe xs >>= (\xs -> return (x : xs))


readSids "none" = Just []
readSids s = joinMaybe (fmap readMaybe (splitOn "," s))

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
parseTracks x = doit x
  where
    doit [] = Right []
    doit (x : xs) =
         do
           t <- (case (readTrack x) of
              Nothing -> Left $ "Error, can't parse: "++x
              Just t -> Right t)
           ts <- parseTracks xs
           Right (t:ts)
                          

--splits a list by a marker into sublists
splitList :: (a -> Bool) -> [a] -> [[a]]
splitList f [] = [[]]
splitList f (x : xs) | (f x) = [] : splitList f xs
                     | True  =
                       let (s : ss) = (splitList f xs)
                           in ((x:s) : ss)
                              



isOption x = (isPrefixOf "-" x)



--parses flags and options meant for mpv, returns State with
-- remaining args
--CAVEAT: options must use --<opt>=<value> format
parseMpvOptions :: StateT [String] (Either String) ([MpvFlag],[MpvOption])
parseMpvOptions =
   do
     args <- get
     (_, (flags, options, rem_args)) <- (runStateT doit ([],[],args))
     put rem_args
     return (reverse flags,reverse options)
    where
      removeDash ('-' : '-' : xs) = xs
      removeDash ('-' : xs) = xs
      parseMpvOption x =
        let opt = removeDash x
            in
                do
                  eqlPos <- elemIndex '=' opt
                  return (take eqlPos opt, drop (eqlPos + 1) opt)
      doit =
        do
           (flags,options,args) <- (get)
           case args of
                [] -> return () --no args left
                (x : xs) | (not (isOption x)) -> return () --end of option args
                (x : xs) -> case parseMpvOption x of
                                Nothing -> put ( (removeDash x): flags, options, xs) >> doit
                                Just o -> put (flags, o : options, xs) >> doit
        

parseMpvArgs :: [String] -> Either String MpvArgs
parseMpvArgs args =
  do
    (mpvargs, _) <- (runStateT doit args)
    return mpvargs
  where
    doit :: StateT [String] (Either String) MpvArgs
    doit =
      do
         (flags,opts) <- parseMpvOptions 
         singleArgs <- get
         return $ MpvArgs flags opts singleArgs

parseArgs :: [String] -> Either String Conf
parseArgs args = do
  splitArgs <- return (splitList (== "--") args)
  if (length splitArgs) /= 3 then (Left "Args must be in format: <srt subtitle files> -- <tracks> -- <mpv args>") else Right ()
  (subfiles : tracksStr : mpvArgsStr : []) <- return splitArgs
  tracks <- parseTracks tracksStr
  mpvArgs <- parseMpvArgs mpvArgsStr
  return $ Conf subfiles tracks mpvArgs

--creates loop arrays, but does not add gap (which must be done after loops are sorted
--in playback order)
createLoopArraysForTrack :: [[Srt]] -> Track -> [EventLoop]
createLoopArraysForTrack srtss t =
  let timingSid = 
        case (sids t) of
           [] -> 1
           x : xs -> x
      srts = srtss !! (timingSid-1)
  in
    fmap (\srt -> createEventLoop t (SF.startTime srt) (SF.endTime srt)) srts
                         
createLoopArrays :: [[Srt]] -> [Track] -> [[EventLoop]]
createLoopArrays srtss tracks = fmap (createLoopArraysForTrack srtss) tracks

--the guaranteed gap betwen subtitles (so we don't flicker other subtitles on the screen)
--during the inbetween times
srtGap = 0.1

addGapsToLoops :: [EventLoop] -> [EventLoop]
addGapsToLoops els =
               let tels = (fmap (addTailGap els) [0..(length els)-1])
               in
                 fmap (addHeadGap tels) [0..(length tels)-1]
 where
    addTailGap :: [EventLoop] -> Int -> EventLoop
    addTailGap els index =
               let el = els !! index
                   nel = earliestLaterTime (endTime el) els index
                   wantedEndTime = (endTime el) + (tailSecs (val el))
                in el { endTime = min wantedEndTime (nel - srtGap) }
    addHeadGap :: [EventLoop] -> Int -> EventLoop
    addHeadGap els index =
               let el = els !! index
                   pel = latestEarlierTime (startTime el)  els index
                   wantedStartTime = (startTime el) - (leadSecs (val el))
                 in el { startTime = max wantedStartTime (pel + srtGap) }
    --TODO PERF goes through entire list for each element
    earliestLaterTime :: Double -> [EventLoop] -> Int -> Double
    earliestLaterTime endTime els index =
                      foldl (\b t -> if t >= endTime then (min t b) else b)
                            9999999.0 
                            (fmap startTime (drop index els))
    latestEarlierTime :: Double -> [EventLoop] -> Int -> Double
    latestEarlierTime startTime els index =
                      foldl (\b t -> if t <= startTime then (min t b) else b)
                            0
                            (fmap endTime (take index els))

createAndSortLoopArrays :: [[Srt]] -> [Track] -> [EventLoop]
createAndSortLoopArrays srts tracks = (addGapsToLoops 
                                        (sortLoopsForPlay
                                          (createLoopArrays srts tracks)))

--TODO 1.5 does not work with baked in subs
