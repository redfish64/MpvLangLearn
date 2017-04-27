module EventLoop (eventLoop,createEventLoop,EventLoop,createInitialELState, ELState(..), ELWaitEvent(..),Track(..)) where

import Control.Monad.State
import Loops
import Util
import Data.List
import Text.Printf (printf)

type EventLoop = Loop Track

data Track = Track { sids :: [Int], speed :: Double, leadSecs :: Double, tailSecs :: Double}
     deriving (Show)

data ELStatus =
    ELStart --begin
  | ELInLoop -- loop action run and inside of a loop
  | ELUserSeek ELStatus -- user did a seek, contains previous status
  | ELBeforeLoop -- defaultNoSrtAction ran and before next loop
  | ELEndLoop -- loop finished, and loopIndex changed, but
               -- next action not run (which would be defaultNoSrtAction or the
               -- next loops action, if outside or inside another loop respectively)
  | ELShutdown -- in shutdown state (the user quit)
  deriving (Show, Eq)

data ELWaitEvent =
     ELWShutdown  -- occurs when the user quit
     | ELWSeekFinished -- occurs when a seek occurred, possibly by the user
                       -- (this may also occur in response to a "seekAction" request
                       --  and this module will be able to handle it properly)
     | ELWSubDelayChanged Double -- the subtitle delay was changed by the user
     | ELWOther -- we woke up from the wait for another reason (will be ignored by this module)
     deriving (Show, Eq)

data ELState m = ELState {
              loopIndex :: Int, -- current index into loops
              loops :: [EventLoop], --list of srt loops
              status :: ELStatus,
              subDelay :: Double,

              --TODO the following never change, reader?
              defaultNoSrtAction :: m (), -- if a loop finishes and there isn't another
                                       -- loop starting, we do this default action here
              readTimeAction :: m (Maybe Double), -- returns the current time. May return nothing
                                              -- if unknown
              readSpeedAction :: m (Maybe Double), -- returns the current speed.
              waitAction :: Double -> m ELWaitEvent, -- waits the given period of time (or less)
                                              -- returns true if user requested shutdown
              seekAction :: Double -> m (), --action when we need to seek to a particular time
              playAction :: EventLoop -> m (), --action to play the loop
              elLog :: String -> m () --log message
           }

instance Show (ELState m) where
  show s = "ELState { loopIndex="++show(loopIndex s)++", status="++show(status s)++" }"

type ELM m = StateT (ELState m) m

createEventLoop track startTime endTime = Loop track startTime endTime

updateEventLoopsForSeek :: Double -> ELState m -> ELState m
updateEventLoopsForSeek currTime state =
  let (Just newLoopIndex) = findIndex (\l -> (endTime l) - maxLoopEndTimeDiff > currTime) (loops state)
      in
        (state { loopIndex = newLoopIndex })

--reads the current playback time, subtracting the offset of the sub delay (set by user
-- hitting 'x' or 'z')
readTimeOrDefaultOffsetSubDelay :: Monad m => Double -> StateT (ELState m) m Double
readTimeOrDefaultOffsetSubDelay def =
  do
    st <- get
    mtime <- lift $ readTimeAction st
    return $ (maybeDefault mtime def) - (subDelay st)


maxLoopStartTimeDiff :: Double
maxLoopStartTimeDiff = 1.0

maxLoopEndTimeDiff = 3.0

                     
eventLoop :: Monad m => ELState m -> m ()
--event_loop = undefined
eventLoop elState = runStateT doit elState >> return ()
  where
    waitAndLoop :: Monad m => Double -> ELM m () --StateT (ELState m) m ()
    waitAndLoop wait_time =
      do
        st <- get
        mspeed <- lift $ (readSpeedAction st)
        let speed = maybeDefault mspeed 1.0
        waitEvent <- lift $ (waitAction st (wait_time/speed))
        case waitEvent of
             ELWShutdown -> (put (st { status = ELShutdown }))
             ELWSubDelayChanged v ->
              do (put (st { subDelay = v}))
                 lift $ (elLog st ("subdelay changed to "++(show v)))
             _ -> (return ())

    --return true if waited, or false if didn't
    waitForTime :: Monad m => Double -> Double -> ELM m Bool
    waitForTime ct et =
      let timeToWait = (et-ct)
          in
            do
              if (timeToWait > 0)
              then
                waitAndLoop timeToWait >> return True
              else
                lift $ return False

    --extract result from a monad op and then branch based on the result
    ifextract :: Monad m => (ELM m a) -> (a -> Bool) -> (ELM m b) -> (ELM m b) -> (ELM m b)
    ifextract cond f tru fals =
        cond >>= (\res -> if (f res) then tru else fals)

    outOfSyncStartTime = 1
    outOfSyncEndTime = 1

    --to determine if we are out of sync in before loop, we have to look for the
    --latest loop that ends before the current loop starts
    endTimeOfLatestPrevLoopEndingBefore :: [EventLoop] -> Int -> Double
    endTimeOfLatestPrevLoopEndingBefore els index =
        let lStartTime = (startTime (els !! index))
        in
          doit2 lStartTime els (index - 1)
        where
          doit2 _ els (-1) = -9999999
          doit2 lStartTime els ci =
              let cl = els !! ci
                  clEndTime = (endTime cl)
              in
                if clEndTime <= lStartTime then
                    clEndTime
                else
                    doit2 lStartTime els (ci-1)
                         
    outOfSyncTime :: Monad m => Double -> ELState m -> Bool
    outOfSyncTime time state =
      let li = (loopIndex state)
          loop = (loops state) !! li
      in
        case (status state) of
          ELInLoop ->
            (time < (startTime loop) - outOfSyncStartTime ||
            time > (endTime loop) + outOfSyncEndTime)
          ELEndLoop -> False
          ELBeforeLoop ->  --if we are before the loop, we should be between
                           --the last loop end and this loop start
            let lastLoopEndTime = endTimeOfLatestPrevLoopEndingBefore (loops state) li
            in
              (time > (startTime loop) + outOfSyncStartTime --not past start of the current loop
              ||
               time < lastLoopEndTime - outOfSyncEndTime -- not before the end of the last prior loop that
                                                         -- ends before us
              )

          --in any other state, we assume we're not out of sync
          _ -> False

    doit :: Monad m => ELM m ()
    doit =
      do
        st <- get
--        time <- lift $ mpv_get_property_double ctx "time-pos"
        time <- readTimeOrDefaultOffsetSubDelay 0.0

        --if we aren't where we expect in the file, the user probably seeked somewhere else
        if outOfSyncTime time st
        then
          do
            let newSt = updateEventLoopsForSeek time st
              in
                do
                  lift $ (defaultNoSrtAction st)
                  put $ newSt {status=ELBeforeLoop}
                  loop <- return $ (loops st) !! (loopIndex st)
                  newLoop <- return $ (loops newSt) !! (loopIndex newSt)
                  lift $ (elLog newSt (printf "outOfSyncTime: time %0.2f status %s lastLoopEndTime %0.2f loop %s new loop %s" time (show (status st)) (endTimeOfLatestPrevLoopEndingBefore (loops st) (loopIndex st)) 
                                                  (show loop)(show newLoop)))

        else
          do
            loop <- return $ (loops st) !! (loopIndex st)
            lift $ (elLog st (printf "not outOfSyncTime: time %0.2f status %s loop %s" time (show (status st)) (show loop)))

        st <- get -- we might have updated it if out of sync, so we reget it here
              
        --find the current loop
        loop <- return $ (loops st) !! (loopIndex st)

        --perform the next action depending on the state
        case (status st) of
            ELStart ->
                do
                  lift $ (defaultNoSrtAction st)
                  put $ st {status=ELBeforeLoop}
            ELBeforeLoop ->
                do
                  --if we are at the start of the next loop
                  ifextract (waitForTime time (startTime loop)) not
                    --do the loop play action
                    (do
                      lift $ (playAction st loop)
                      put $ st {status=ELInLoop})
                    (return ())
            ELInLoop ->
                do
                  --if we wait at all, we go back to the start and reread the time, etc.
                  --otherwise, we go ahead and update the state
                  ifextract (waitForTime time (endTime loop)) not
                    (do
                       put $ st {status=ELEndLoop,
                                 loopIndex = (loopIndex st) + 1
                                })
                    (return ())
            ELEndLoop ->
                do
                  if time < (startTime loop) -- if the next loop doesn't start yet
                  then
                    do
                      --go to default mode and let it play
                      lift $ (defaultNoSrtAction st)
                      put $ st {status=ELBeforeLoop}
                  else
                    do
                      --seek (backwards) to the start of the loop and play it
                      lift $ (seekAction st ((startTime loop)+(subDelay st))) >> (playAction st loop)
                      put $ st {status=ELInLoop}
            ELShutdown -> return ()

        if (status st) == ELShutdown then return ()
        else doit --restart the event loop

bigTime = 99999999

createInitialELState loops defaultNoSrtAction readTimeAction waitAction seekAction playAction =
  ELState 0
           (loops ++ [(Loop (Track [] 1.0 0.0 0.0) bigTime bigTime)]) -- we add an ending loop which keeps the system playing until the end of the movie
           ELStart
           0.0 --sub delay
           defaultNoSrtAction
           readTimeAction
           waitAction
           seekAction
           playAction

