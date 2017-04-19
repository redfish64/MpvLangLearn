module EventLoopTest where

import EventLoop
import Control.Monad.RWS
import Text.Printf

data WaitActionStrategy = WasOneSecondOrLess

data TestEnv = TestEnv { waitActionStrategy :: WaitActionStrategy }

data TestState = TestState { currTime :: Maybe Double, --this is play time, so modified by speed
                             videoLength :: Double, subTrackDisplayed :: Maybe Int,
                             speed :: Double --playback speed
                           }

type TestM = RWS () [String] TestState ()

loops1 = []

defaultNoSrtAction1 =
  tell "defaultNoSrtAction1" >> modify $ \st -> st { speed = 1.0, subTrackDisplayed = Nothing }
readTimeAction1 = do
                    st <- get
                    tell $ "readTimeAction1: " ++ (currTime st)
                    return (currTime st)
                    
waitAction1 time = do
  st <- get
  env <- ask
  let playTime = (calcWaitTime st env)
  put $ st {currTime = (currTime st) + playTime}
  tell .printf "waitAction1: req wait time %.2f, speed %.2f, actual wait time %.2f" $ time (speed st) (playTime st)
  where
    calcWaitTime st env =
      case (waitActionStrategy env) of
        WasOneSecondOrLess -> (min (time / speed st) 1.0)
      
  
  (putStrLn $ "waitAction1 "++(show time)) >> return True
seekAction1 loop = putStrLn $ "seekAction1 "++(show loop)
playAction1 loop = putStrLn $ "playAction1 "++(show loop)

st1 = createInitialMpvState loops1 defaultNoSrtAction1 readTimeAction1 waitAction1 seekAction1
                            playAction1
