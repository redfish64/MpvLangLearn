module EventLoopTest where

import qualified EventLoop as EL
import EventLoop (MpvLoop)
import Control.Monad.RWS
import Text.Printf
import Loops
import Util

data WaitActionStrategy = WASLimit Double -- wait up to a maximum amount

data TestEnv = TestEnv { waitActionStrategy :: WaitActionStrategy }

data TestState = TestState { currTime :: Maybe Double, --this is play time, so modified by speed
                             videoLength :: Double,
                             sid :: Maybe Int,
                             speed :: Double --playback speed
                           } deriving (Show)

type TestM = RWS TestEnv[(String,TestState)] TestState

tlog :: String -> TestM ()
tlog msg = do
  st <- get
  tell [(msg,st)]

defaultNoSrtAction1 :: TestM ()
defaultNoSrtAction1 =
  do
    modify $ \st -> st { speed = 1.0, sid = Nothing }
    tlog "defaultNoSrtAction1"
readTimeAction1 = do
                    st <- get
                    tlog "readTimeAction1"
                    return (currTime st)
waitAction1 :: Double -> TestM Bool
waitAction1 time = do
  st <- get
  env <- ask
  let playTime = (calcWaitTime st env)
  let (Just jcurrTime) = if (currTime st) == Nothing then Just 0.0 else (currTime st)
  let newTime = jcurrTime + playTime
  put $ st {currTime = Just $ newTime}
  let exit = newTime > (videoLength st)
  tlog (printf "waitAction1 re %.2f ac %.2f" time playTime)
  return exit
  where
    calcWaitTime st env =
      case (waitActionStrategy env) of
        WASLimit limit -> (min (time / (speed st)) limit)
        
seekAction1 :: MpvLoop -> TestM ()        
seekAction1 loop = do
  st <- get
  modify $ \st -> st { currTime = Just (startTime loop) }
  newSt <- get
  tlog (printf "seekAction1 %s" (show (currTime newSt)))
  return ()
  
playAction1 :: MpvLoop -> TestM ()        
playAction1 loop = do
  do
    modify $ \st -> st { speed=(EL.speed . val $ loop) , sid = (EL.sid . val $ loop) }
    tlog "playAction1"

st1 :: EL.MpvState TestM
st1 = EL.createInitialMpvState loops1 defaultNoSrtAction1 readTimeAction1 waitAction1
                               seekAction1 playAction1

loops1 = fmap (\(st,ed,speed,sid) -> EL.createMpvLoop st ed speed sid)
  [
    (5.0,10.0,1.0,Just 1)
  , (15.0,25.0,0.95,Just 1)
  , (15.0,25.0,0.85,Just 2)
  ]


test1 = runRWS (EL.event_loop st1) (TestEnv $ WASLimit 30.0)
                       (TestState Nothing 30.0 Nothing 1.0)


run (res,state,writer) = writer

pt writer = 
    do
      recurseMonad writer (\(s,t) -> putStrLn $ printf "%-30s %s" s (show t))


--to test, for example, run:
--  pt $ take 10 (run test1)
