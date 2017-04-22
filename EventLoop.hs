module EventLoop (eventLoop,createEventLoop,EventLoop,speed,sids,createInitialELState, ELState(..)) where

import Control.Monad.State
import Loops
import Util

data EventLoopData = EventLoopData { speed :: Double, sids :: [Int] } deriving (Show)

type EventLoop = Loop EventLoopData

data ELStatus =
    ELStart --begin
  | ELInLoop -- loop action run and inside of a loop
  | ELOutOfLoop -- defaultNoSrtAction run and outside of loop
  | ELEndLoop -- loop finished, and first of nextLoops moved to priorLoops, but
               -- next action not run (which would be defaultNoSrtAction or the
               -- next loops action, if outside or inside another loop respectively)
  | ELShutdown -- in shutdown state (the user quit)
  deriving (Show, Eq)

data ELState m = ELState {
              priorLoops :: [EventLoop], --loops already run
              nextLoops :: [EventLoop], --loops to run later (or currently running)
              status :: ELStatus,
              defaultNoSrtAction :: m (), -- if a loop finishes and there isn't another
                                       -- loop starting, we do this default action here
              readTimeAction :: m (Maybe Double), -- returns the current time. May return nothing
                                              -- if unknown
              readSpeedAction :: m (Maybe Double), -- returns the current speed.
              waitAction :: Double -> m Bool, -- waits the given period of time (or less)
                                              -- returns true if user requested shutdown
              seekAction :: EventLoop -> m (), --action when we need to seek to the start of a loop 
              playAction :: EventLoop -> m () --action to play the loop
           }

instance Show (ELState m) where
  show s = "ELState { priorLoops="++show(priorLoops s)++", nextLoops="++show(nextLoops s)++", status="++show(status s)++" }"

type ELM m = StateT (ELState m) m

createEventLoop startTime endTime speed sids = Loop (EventLoopData speed sids) startTime endTime

eventLoop :: Monad m => ELState m -> m ()
--event_loop = undefined
eventLoop mpvState = runStateT doit mpvState >> return ()
  where
    waitAndLoop :: Monad m => Double -> StateT (ELState m) m ()
    waitAndLoop wait_time =
      do
        st <- get
        mspeed <- lift $ (readSpeedAction st)
        let speed = maybeDefault mspeed 1.0
        ifextract (lift $ (waitAction st) $ wait_time/speed) id
          (put (st { status = ELShutdown }))
          (return ())
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

    doit :: Monad m => ELM m ()
    doit =
      do
        st <- get
--        time <- lift $ mpv_get_property_double ctx "time-pos"
        time <- lift $ (readTimeAction st)
        jtime <- return $ case time of
             Nothing -> 0.0
             (Just t) -> t

        --find the current loop
        loop <- return (head (nextLoops st))

        --perform the next action depending on the state
        case (status st) of
          ELStart ->
              do
                lift $ (defaultNoSrtAction st)
                put $ st {status=ELOutOfLoop}
          ELOutOfLoop ->
              do
                ifextract (waitForTime jtime (startTime loop)) not
                  (do
                    lift $ (playAction st loop)
                    put $ st {status=ELInLoop})
                  (return ())
          ELInLoop ->
              do
                --if we wait at all, we go back to the start and reread the time, etc.
                --otherwise, we go ahead and update the state
                ifextract (waitForTime jtime (endTime loop)) not
                  (do
                     put $ st {status=ELEndLoop,
                               priorLoops = loop : (priorLoops st),
                               nextLoops = tail (nextLoops st)
                              })
                  (return ())
          ELEndLoop ->
              do
                if jtime < (startTime loop) -- if the next loop doesn't start yet
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
  ELState []
           (loops ++ [(Loop (EventLoopData 1.0 []) bigTime bigTime)]) -- we add an ending loop which keeps the system playing until the end of the movie
           ELStart
           defaultNoSrtAction
           readTimeAction
           waitAction
           seekAction
           playAction

