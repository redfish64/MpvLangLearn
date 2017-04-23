module EventLoop (eventLoop,createEventLoop,EventLoop,speed,sids,createInitialELState, ELState(..), ELWaitEvent(..)) where

import Control.Monad.State
import Loops
import Util
import Data.List

data EventLoopData = EventLoopData { speed :: Double, sids :: [Int] } deriving (Show)

type EventLoop = Loop EventLoopData

data ELStatus =
    ELStart --begin
  | ELInLoop -- loop action run and inside of a loop
  | ELUserSeek ELStatus -- user did a seek, contains previous status
  | ELOutOfLoop -- defaultNoSrtAction run and outside of loop
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
     | ELWOther -- we woke up from the wait for another reason (will be ignored by this module)
     deriving (Show, Eq)

data ELState m = ELState {
              loopIndex :: Int, -- current index into loops
              loops :: [EventLoop], --list of srt loops
              status :: ELStatus,

              --TODO the following never change, reader?
              defaultNoSrtAction :: m (), -- if a loop finishes and there isn't another
                                       -- loop starting, we do this default action here
              readTimeAction :: m (Maybe Double), -- returns the current time. May return nothing
                                              -- if unknown
              readSpeedAction :: m (Maybe Double), -- returns the current speed.
              waitAction :: Double -> m ELWaitEvent, -- waits the given period of time (or less)
                                              -- returns true if user requested shutdown
              seekAction :: EventLoop -> m (), --action when we need to seek to the start of a loop 
              playAction :: EventLoop -> m () --action to play the loop
           }

instance Show (ELState m) where
  show s = "ELState { loopIndex="++show(loopIndex s)++", status="++show(status s)++" }"

type ELM m = StateT (ELState m) m

createEventLoop startTime endTime speed sids = Loop (EventLoopData speed sids) startTime endTime

updateEventLoopsForSeek :: Double -> ELState m -> ELState m
updateEventLoopsForSeek currTime state =
  let (Just newLoopIndex) = findIndex (\l -> (endTime l) - maxLoopEndTimeDiff > currTime) (loops state)
      in
        (state { loopIndex = newLoopIndex })
        
readTimeOrDefault :: Monad m => Double -> StateT (ELState m) m Double
readTimeOrDefault def =
  do
    st <- get
    mtime <- lift $ readTimeAction st
    return $ maybeDefault mtime def


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
             -- ELWSeekFinished ->
             --   do
             --     time <- readTimeOrDefault 0.0
             --     --the user may have seeked or we may be responding to our
             --     --own seek call. In the latter case, we should only be seeking
             --     --to the start of our own event. In the former case, we could
             --     --be seeking anywhere. The important part is that we don't
             --     --"get in the way" of the user's intentions. This means that
             --     --we shouldn't trigger a rewind at the end of a subtitle
             --     --if the user skipped to it
                 
             --     let currLoop = (head (nextLoops st))

             --     --if the rewind event is within the current subtitle
             --     --start and end times (within certain tolerances), we
             --     --ignore it
             --     if time > (startTime currLoop) - maxLoopStartTimeDiff &&
             --       time < (endTime currLoop) - maxLoopEndTimeDiff then
             --       return ()
             --     else
             --       do
             --         modify (updateEventLoopsForSeek time)
             --         return ()


             _ -> (return ())
        -- (mpv_wait_event (ctx st) $ realToFrac wait_time) >>= peek

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

    outOfSyncTime :: Monad m => Double -> ELState m -> Bool
    outOfSyncTime time state =
      let li = (loopIndex state)
          loop = (loops state) !! li
          lastLoopEndTime =
             if li == 0
             then -999999.00
             else
                endTime ((loops state) !! (li - 1))
      in
        case (status state) of
          ELInLoop ->
            (time < (startTime loop) - outOfSyncStartTime ||
            time > (endTime loop) + outOfSyncEndTime)
          ELEndLoop -> False
          ELOutOfLoop -> 
            (time > (startTime loop) + outOfSyncStartTime ||
             time < lastLoopEndTime - outOfSyncEndTime)
          _ -> False

    doit :: Monad m => ELM m ()
    doit =
      do
        st <- get
--        time <- lift $ mpv_get_property_double ctx "time-pos"
        time <- readTimeOrDefault 0.0

        --find the current loop
        loop <- return $ (loops st) !! (loopIndex st)

        --if we aren't where we expect in the file, the user probably seeked somewhere else
        if outOfSyncTime time st
        then
          let newSt = updateEventLoopsForSeek time st
            in
              do
                lift $ (defaultNoSrtAction st)
                put $ newSt {status=ELOutOfLoop}
        else              
          --perform the next action depending on the state
          case (status st) of
            ELStart ->
                do
                  lift $ (defaultNoSrtAction st)
                  put $ st {status=ELOutOfLoop}
            -- ELUserSeek oldStatus ->
            --     do
            --       if(time < (startTime loop)- 4) -- if now outside of loop
            --       then
            --         do 
            --           put $ st {status = ELOutOfLoop}
            --           if(oldStatus == ELOutOfLoop)
            --           then
            --             return ()
            --           else
            --             lift $ (defaultNoSrtAction st)
            --       else -- must be inside the current loop
            --         do
            --           lift $ (playAction st loop)
            --           put $ st { status=ELInLoop}

            ELOutOfLoop ->
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
                      put $ st {status=ELOutOfLoop}
                  else
                    do
                      --seek (backwards) to the start of the loop and play it
                      lift $ (seekAction st loop) >> (playAction st loop)
                      put $ st {status=ELInLoop}
            ELShutdown -> return ()

        if (status st) == ELShutdown then return ()
        else doit --restart the event loop

bigTime = 99999999

createInitialELState loops defaultNoSrtAction readTimeAction waitAction seekAction playAction =
  ELState 0
           (loops ++ [(Loop (EventLoopData 1.0 []) bigTime bigTime)]) -- we add an ending loop which keeps the system playing until the end of the movie
           ELStart
           defaultNoSrtAction
           readTimeAction
           waitAction
           seekAction
           playAction

