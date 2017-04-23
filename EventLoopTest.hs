module EventLoopTest where

import qualified EventLoop as EL
import EventLoop (EventLoop)
import Control.Monad.RWS
import Text.Printf
import Loops
import Util

data WaitActionStrategy = WASLimit Double -- wait up to a maximum amount

data TestEnv = TestEnv { waitActionStrategy :: WaitActionStrategy }

data TestState = TestState { currTime :: Maybe Double, --this is play time, so modified by speed
                             videoLength :: Double,
                             sids :: [Int],
                             speed :: Double, --playback speed
                             userSeeks :: [(Double,Double)] --list of user seeks, (from,to)
                           } deriving (Show)

type TestM = RWS TestEnv [(String,TestState)] TestState

tlog :: String -> TestM ()
tlog msg = do
  st <- get
  tell [(msg,st)]

defaultNoSrtAction1 :: TestM ()
defaultNoSrtAction1 =
  do
    modify $ \st -> st { speed = 1.0, sids = [] }
    tlog "defaultNoSrtAction1"

readTimeAction1 = do
                    st <- get
                    tlog "readTimeAction1"
                    return (currTime st)

readSpeedAction1 = do
                    st <- get
                    tlog "readSpeedAction1"
                    return $ Just (speed st)

waitAction1 :: Double -> TestM EL.ELWaitEvent
waitAction1 time = do
  st <- get
  env <- ask
  let playTime = (calcWaitTime st env)
  let (Just jcurrTime) = if (currTime st) == Nothing then Just 0.0 else (currTime st)
  let newTime = jcurrTime + playTime
  let (userSkipFrom,userSkipTo) = case (userSeeks st) of
        [] -> (99999999.0,99999999.0)
        x : _ -> x

  let newTime2 = if newTime > userSkipFrom
      then userSkipTo
      else newTime

  let waitEvent = if (newTime2 > (videoLength st))
      then EL.ELWShutdown
      else if newTime2 /= newTime then EL.ELWSeekFinished
      else EL.ELWOther

  let newUserSeeks = (if newTime2 /= newTime then (tail (userSeeks st)) else (userSeeks st))
  
  put $ st {currTime = Just $ newTime2, userSeeks = newUserSeeks}

  tlog (printf "waitAction1 re %.2f ac %.2f %s" time newTime2 (show waitEvent))
  return waitEvent
 where
    calcWaitTime st env =
      case (waitActionStrategy env) of
        WASLimit limit -> (min (time * (speed st)) limit)
        
seekAction1 :: EventLoop -> TestM ()        
seekAction1 loop = do
  st <- get
  modify $ \st -> st { currTime = Just (startTime loop) }
  newSt <- get
  tlog (printf "seekAction1 %s" (show (currTime newSt)))
  return ()
  
playAction1 :: EventLoop -> TestM ()        
playAction1 loop = do
  do
    modify $ \st -> st { speed=(EL.speed . val $ loop) , sids = EL.sids . val $ loop }
    tlog "playAction1"

st1 :: EL.ELState TestM
st1 = EL.createInitialELState loops1 defaultNoSrtAction1 readTimeAction1
                               readSpeedAction1 waitAction1
                               seekAction1 playAction1

loops1 = fmap (\(st,ed,speed,sid) -> EL.createEventLoop st ed speed sid)
  [
    (5.0,10.0,1.0, [1])
  , (15.0,25.0,0.95,[1])
  , (15.0,25.0,0.85,[2,1])
  ]


test1 = runRWS (EL.eventLoop st1) (TestEnv $ WASLimit 30.0)
                       (TestState Nothing 30.0 [] 1.0 
                                  [] --[(20.0,27.0)] --user seeks from,to
                       )

loops2 = fmap (\(st,ed,speed,sid) -> EL.createEventLoop st ed speed sid)
  [
    (5.0,10.0,1.0, [])
  , (5.0,10.0,1.0, [1])
  , (15.0,25.0,0.95,[])
  , (15.0,25.0,0.95,[1])
  , (35.0,40.0,0.95,[])
  , (35.0,40.0,0.95,[1])
  , (45.0,55.0,0.95,[])
  , (45.0,55.0,0.95,[1])
  ]

st2 = EL.createInitialELState loops2 defaultNoSrtAction1 readTimeAction1
                               readSpeedAction1 waitAction1
                               seekAction1 playAction1

test2 = runRWS (EL.eventLoop st2) (TestEnv $ WASLimit 30.0)
                       (TestState Nothing 30.0 [] 1.0 
                                  [(20.0,3.0)] --user seeks from,to
                       )


run (res,state,writer) = writer

pt writer = 
    do
      recurseMonad writer (\(s,t) -> putStrLn $ printf "%-30s %s" s (show t))

pt1 = pt (run test1)

pt2 = pt (run test2)
