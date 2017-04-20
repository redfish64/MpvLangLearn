module Main where

import Init
import System.Environment
import MpvLL
import Data.List.Split (splitOn)
import Data.List (isPrefixOf,elemIndex)
import Text.Read (readMaybe)
import Control.Exception.Base (Exception,throwIO)
import SrtFile (loadSrtFile)

import Foreign (Ptr,peek)
import MpvStructs
import EventLoop
import Util

data MyException = MyException String deriving (Show)
instance Exception MyException 


     
runit :: Conf -> IO ()
runit conf = 
  do
    ctx <- mpv_create
    putStrLn "created context"
    mpv_set_option_string ctx "input-default-bindings" "yes"
    mpv_set_option_string ctx "input-vo-keyboard" "yes"
    mpv_set_option_flag ctx "osc" 1
    putStrLn "set flags for subfiles"
    recurseMonad (subfiles conf) (\sf -> mpv_set_option_string ctx "sub-file" sf )
    putStrLn "set options"
    setupMpvFlags ctx (flags (mpvArgs conf))
    setupMpvOptions ctx (opts (mpvArgs conf))
    mpv_initialize ctx
    putStrLn "initialized"
    --TODO if file doesn't exist, doesn't report an error
    loadFiles ctx (singleArgs (mpvArgs conf))
    putStrLn "loaded files"
--    event_loop ctx undefined
    putStrLn "finished event loop"
    mpv_terminate_destroy ctx -- this should be in some sort of failsafe (like java finally)
    return ()

main :: IO ()
main =
  do
    argsStr <- getArgs
    c <- case (parseArgs argsStr) of
                 Left s -> throwIO $ MyException $ "Error: " ++ s
                 Right c -> return c
    srtArrays <- doMonadOnList (subfiles c) loadSrtFile
--    loopArrays <- return createLoopArrays srtArrays (tracks c)
    runit c
    
-- createLoopArraysForTrack :: [[Srt]] -> Track -> [Loop]
-- createLoopArraysForTrack srtss t =
--   case (subIds t) of
--     [] -- no srt at all, the default case

-- createLoopArrays :: [[Srt]] -> [Track] -> [[Loop]]
-- createLoopArrays srtss [] = []
-- createLoopArrays srtss (t : tracks) =
                       

doMonadOnList :: Monad m => [a] -> (a -> m b) -> (m [b])
doMonadOnList [] _ = return []
doMonadOnList (a : as) f =
   do
     b <- (f a)
     bs <- doMonadOnList as f
     return (b : bs)
                                                        
