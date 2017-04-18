module Main where

import Init
import System.Environment
import MpvLL
import Control.Monad.State
import Data.List.Split (splitOn)
import Data.List (isPrefixOf,elemIndex)
import Text.Read (readMaybe)
import Control.Exception.Base (Exception,throwIO)
import SrtFile (loadSrtFile)
import Loops
import Control.Monad.Trans.Either

import Foreign (Ptr,peek)
import MpvStructs

data MyException = MyException String deriving (Show)
instance Exception MyException 



type MpvLoop = Loop (IO ())

data MpvState = MpvState {
              ctx :: Ptr Ctx,
              priorLoops :: [MpvLoop], --loops already run
              nextLoops :: [MpvLoop], --loops to run later
              currLoop :: Maybe MpvLoop, --if processing a current loop that has started
                                    --but not finished, this is it
              defaultNoSrtAction :: IO () -- if a loop finishes and there isn't another
                                       -- loop starting, we do this default action here
           }


--in several cases, we need to break out of the main loop and do something else
--such as wait for a particular time, or handle a shutdown request, etc
--We use an EitherT to support this. left means to break out and doing something
--other than the main loop action
--returns true if should loop again
type MpvET = StateT MpvState (EitherT (IO Bool) IO) Bool

getActiveLoop st = undefined

event_loop :: MpvState -> IO ()
--event_loop = undefined
event_loop mpvState =
  do
    x <- runEitherT (runStateT doit mpvState)
    case x of
       (Left c) ->
         do
           res <- c
           if res Then event_loop 
       --true we loop again
       (Right (True, mpvState)) -> event_loop mpvState
       --false we quit out
       (Right (False, _)) -> return ()
    return ()
  where
    -- --runs action and quits if Bool is true
    breakif :: Bool -> MpvET -> MpvET
    breakif True action = action >>= (\res -> lift (left (return res)))
    breakif False _ = lift $ return True

    -- --runs action and quits if Bool is true
    breakif2 :: Bool -> IO Bool -> MpvET
    breakif2 True action = lift $ left action
    breakif2 False _ = lift $ return True

    -- --waits the given amount of time (or until the next event) and
    -- --recalls doit
    waitAndLoop :: Double -> MpvET
    waitAndLoop wait_time =
      do
        st <- get
        event <- liftIO $ (mpv_wait_event (ctx st) $ realToFrac wait_time) >>= peek
        breakif2 ((event_id event) == mpvEventShutdown)
           (do
                     putStrLn "Bye!"
                     return False)
          
        --restart the event_loop, so it can recheck, since we may not have waited the
        --complete time because of some other event
        lift $ left $ return True --quit out after we've run doit

    --if wait time is positive, will wait and loop back to start
    --else no-op
    waitForTime :: Double -> Double -> MpvET
    waitForTime = undefined
    -- waitForTime st ct et =
    --   do
    --     timeToWait <- return et - ct
    --     if (timeToWait > 0)
    --       then
    --         waitAndLoop timeToWait
    --       else
    --         right $ return () --no reason to wait, so continue

 
    doit :: MpvET
    doit =
      do
        st <- get
        ctx <- return (ctx st)
        time <- liftIO $ mpv_get_property_double ctx "time-pos"
        breakif (time == Nothing) (waitAndLoop 1)
        (Just jtime) <- return time
        
        (inLoop, loop) <- return (getActiveLoop st)

        waitForTime (realToFrac jtime) (if inLoop then (endTime loop) else (startTime loop))
        return True
        
        
        
     
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
                                                        
