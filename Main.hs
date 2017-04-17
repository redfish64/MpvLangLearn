module Main where

import System.Environment
import MpvLL
import Control.Monad.State
import Data.List.Split (splitOn)
import Data.List (isPrefixOf)

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
                          

data Conf = Conf { tracks :: [Track], mpvArgs :: MpvArgs } deriving (Show)

parseArgs :: [String] -> Either String Conf
parseArgs args = do
  tracks <- parseTracks (takeWhile (not . (== "--")) args)
  mpvArgsStr <- (case (dropWhile (not . (== "--")) args) of
    [] -> Left "args must contain '--'. Args after '--' are directly processed by mpv"
    (x : xs) -> Right xs)
  mpvArgs <- parseMpvArgs mpvArgsStr
  return $ Conf tracks mpvArgs


type MpvFlag = String

type MpvOption = (String,String)

data MpvArgs = MpvArgs { flags :: [MpvFlag], opts :: [MpvOption], singleArgs :: [String] } deriving (Show)



isOption x = (isPrefixOf "-" x)



--parses flags and options meant for mpv, returns State with
-- remaining args
--
--TODO this isn't completely accurate. If an argument to an option starts with a
-- '-' then this will assume that its a flag. The alternative is to find a list
-- of all flags versus options, and this can change per mpv version, so it is what
-- it is
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
      doit =
        do
           (flags,options,args) <- (get)
           case args of
                [] -> return () --no args left
                (x : xs) | (not (isOption x)) -> return () --end of option args
                ( x : y : zs ) | (not (isOption y)) ->
                                 put (flags, ((removeDash x, y) : options), zs) >> doit
                ( x : zs ) -> put ( (removeDash x): flags, options, zs) >> doit
                     
        

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

runit :: Conf -> IO ()
runit conf = 
  do
    ctx <- mpv_create
    putStrLn "created context"
    mpv_set_option_string ctx "input-default-bindings" "yes"
    mpv_set_option_string ctx "input-vo-keyboard" "yes"
    mpv_set_option_flag ctx "osc" 1
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
      
