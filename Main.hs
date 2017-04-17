module Main where

import System.Environment
import MpvLL
import Control.Monad.State
import Data.List.Split (splitOn)
import Data.List (isPrefixOf,elemIndex)
import Text.Read (readMaybe)

data Track = Track { subIds :: [Int], speed :: Float, leadSecs :: Float, tailSecs :: Float}
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
                          

data Conf = Conf { subfiles :: [String], tracks :: [Track], mpvArgs :: MpvArgs } deriving (Show)

--splits a list by a marker into sublists
splitList :: (a -> Bool) -> [a] -> [[a]]
splitList f [] = [[]]
splitList f (x : xs) | (f x) = [] : splitList f xs
                     | True  =
                       let (s : ss) = (splitList f xs)
                           in ((x:s) : ss)
                              


type MpvFlag = String

type MpvOption = (String,String)

data MpvArgs = MpvArgs { flags :: [MpvFlag], opts :: [MpvOption], singleArgs :: [String] } deriving (Show)



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

runit :: Conf -> IO ()
runit conf = 
  do
    ctx <- mpv_create
    putStrLn "created context"
    mpv_set_option_string ctx "input-default-bindings" "yes"
    mpv_set_option_string ctx "input-vo-keyboard" "yes"
    mpv_set_option_flag ctx "osc" 1
    putStrLn "set flags for subfiles"
--    recurseMonad (tracks conf) 
    putStrLn "set options"
    setupMpvFlags ctx (flags (mpvArgs conf))
    setupMpvOptions ctx (opts (mpvArgs conf))
    mpv_initialize ctx
    putStrLn "initialized"
    loadFiles ctx (singleArgs (mpvArgs conf))
    putStrLn "loaded files"
    event_loop ctx
    putStrLn "finished event loop"
    mpv_terminate_destroy ctx -- this should be in some sort of failsafe (like java finally)
    return ()

main :: IO ()
main =
  do
    args <- getArgs
    case (parseArgs args) of
      Right (c)-> runit c
      Left s -> putStrLn $ "Error: " ++ s
      
