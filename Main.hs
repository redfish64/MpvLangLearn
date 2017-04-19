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


data MpvLoopAction = MpvLoopAction
  { seekAction :: MpvLoop -> IO (), --action when we need to seek to the start of a loop 
    playAction :: MpvLoop -> IO () --action to play the loop
  }
  
type MpvLoop = Loop MpvLoopAction

data MpvStatus =
    MpvStart --begin
  | MpvInLoop -- loop action run and inside of a loop
  | MpvOutOfLoop -- defaultNoSrtAction run and outside of loop
  | MpvEndLoop -- loop finished, and first of nextLoops moved to priorLoops, but
               -- next action not run (which would be defaultNoSrtAction or the
               -- next loops action, if outside or inside another loop respectively)
data MpvState = MpvState {
              priorLoops :: [MpvLoop], --loops already run
              nextLoops :: [MpvLoop], --loops to run later (or currently running)
              status :: MpvStatus
              defaultNoSrtAction :: IO () -- if a loop finishes and there isn't another
                                       -- loop starting, we do this default action here
              timeAction :: IO (Maybe Double) -- returns the current time. May return nothing
                                              -- if unknown
           }


--in several cases, we need to break out of the main loop and do something else
--such as wait for a particular time, or handle a shutdown request, etc
--We use an EitherT to support this. left means to break out and doing something
--other than the main loop action
--returns true if should loop again
type MpvET = StateT MpvState (EitherT (Bool,MpvState) IO) Bool


getActiveLoop :: MpvState -> Maybe (Bool,MpvLoop)
getActiveLoop st =
  case (currLoop st) of
    Nothing -> getNextLoop
    Just loop -> Just (True,loop)
  where
    getNextLoop = case (nextLoops st) of
                    [] -> Nothing
                    (l : ls) -> Just (False,l)



event_loop :: MpvState -> IO ()
--event_loop = undefined
event_loop mpvState =
  do 
    x <- runEitherT (runStateT doit mpvState)       
    case x of
       (Left (True,mpvState)) -> event_loop mpvState
       (Left (False,_)) -> return ()
       --true we loop again
       (Right (True, mpvState)) -> event_loop mpvState
       --false we quit out
       (Right (False, _)) -> return ()
    return ()
  where
    -- --runs action and quits if Bool is true
    breakif :: Bool -> MpvET -> MpvET
    breakif True action =
      do
        res <- action
        state <- get
        lift (left (res,state))
    breakif False _ = lift $ return True

    -- --runs action and quits if Bool is true
    -- breakif2 :: Bool -> IO Bool -> MpvET
    -- breakif2 True action = lift $ left action
    -- breakif2 False _ = lift $ return True

    -- --waits the given amount of time (or until the next event) and
    -- --recalls doit
    waitAndLoop :: Double -> MpvET
    waitAndLoop wait_time =
      do
        st <- get
        event <- liftIO $ (mpv_wait_event (ctx st) $ realToFrac wait_time) >>= peek
        breakif ((event_id event) == mpvEventShutdown)
           (lift $ lift (do
                     putStrLn "Bye!"
                     return False))
          
        --restart the event_loop, so it can recheck, since we may not have waited the
        --complete time because of some other event
        lift $ left $ (True,st) --quit out after we've run doit

    --if wait time is positive, will wait and loop back to start
    --else no-op
    waitForTime :: Double -> Double -> MpvET
    waitForTime ct et =
      let timeToWait = (et-ct)
          in
            do
              if (timeToWait > 0)
              then
                waitAndLoop timeToWait
              else
                lift $ return True --no reason to wait, so continue

    restartLoop :: MpvET
    restartLoop =
         do
            st <- get
            return (True, st)


    doit :: MpvET
    doit =
      do
        st <- get
--        time <- liftIO $ mpv_get_property_double ctx "time-pos"
        time <- liftIO $ (timeAction st)
        breakif (time == Nothing) (waitAndLoop 1)
        (Just jtime) <- return (realToFrac time)

        --find the current loop
        nextLoop <- return (head (nextLoops st))

        --perform the next action depending on the state
        case (status st) of
          MpvStart ->
              do
                liftIO $ (defaultNoSrtAction st)
                put $ st {status=MpvOutOfLoop}
          MpvOutOfLoop ->
              do
                waitForTime jtime (startTime loop)
                liftIO $ (playAction (action loop))
                put $ st {status=MpvInLoop}
          MpvInLoop ->
              do
                waitForTime jtime (endTime loop)
                put $ st {status=MpvEndLoop,
                          priorLoops = loop : (priorLoops st),
                          nextLoops = tail (nextLoops st),
                         }
          MpvEndLoop ->
              do
                if jtime < (startTime loop)
                then
                  do
                    liftIO $ (defaultNoSrtAction st)
                    put $ st {status=MpvOutOfLoop}
                else
                  do
                    liftIO $ (seekAction (action loop)) >> (playAction (action loop))
                    put $ st {status=MpvInLoop}

                
          
        
        Just (inLoop, loop) <- return maybeLoop

        --if we are currently processing an event
        if inLoop
        then
          do
             waitForTime (realToFrac jtime) (endTime loop) --wait for the loop to finish

             --move to the next loop
             newSt <- return st { nextLoops = tail (nextLoops st),
                                  priorLoops = (head (nextLoops st)) : (priorLoops st),
                                  status = MpvEndLoop}
             put newSt
             left $ return (True, newSt) --restart
        else return ()

        
        
     
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
                                                        
