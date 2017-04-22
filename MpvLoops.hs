--handles controlling the mpv library on behalf of the event loops
module MpvLoops (createInitialMpvState,
                 MLEnv(..)
                ) where

import qualified MpvFFI as M
import qualified MpvStructs as MS
import Foreign (Ptr,peek)
import qualified EventLoop as EL
import Control.Monad.Reader
import Loops

data MLEnv = MLEnv { ctx :: M.Ctx,
                       defaultNoSrtSpeed :: Double
                     }

type MLM = ReaderT MLEnv IO


setSpeed :: Double -> MLM ()
setSpeed speed =
  do 
    lift $ putStrLn $ "setSpeed: "++(show speed)
    env <- ask
    lift $ M.mpvSetPropertyDouble (ctx env) "speed" (realToFrac speed)
    lift $ putStrLn $ "done setSpeed: "++(show speed)
    return ()

setSids :: [Int] -> MLM ()
setSids sids =
  do
    env <- ask
    let (sid,ssid) = case sids of
                          [] -> ("no","no")
                          --TODO, use int based set option
                          x : [] -> (show x,"no")
                          x1 : x2 : _ -> (show x1, show x2)
    lift $ putStrLn $ "setSids " ++ (show (sid,ssid))
    lift $ do
                M.mpvSetPropertyString (ctx env) "sid" sid
                M.mpvSetPropertyString (ctx env) "secondary-sid" ssid
    return ()


defaultNoSrtAction :: MLM ()
defaultNoSrtAction =
  do
    env <- ask
    setSpeed (defaultNoSrtSpeed env)
    setSids []

readDouble :: String -> MLM (Maybe Double)
readDouble name =
  do
--    lift $ putStrLn $ "readDouble: "++name
    env <- ask
    time <- (lift $ M.mpvGetPropertyDouble (ctx env) name)
--    lift $ putStrLn $ "done readDouble: "++name
    return $ realToFrac <$> time
    
readTimeAction :: MLM (Maybe Double)
readTimeAction = readDouble "playback-time"

readSpeedAction :: MLM (Maybe Double)
readSpeedAction = readDouble "speed"

waitAction :: Double -> MLM Bool
waitAction time =
  do
    env <- ask
    event <- lift $ (M.mpvWaitEvent (ctx env) (realToFrac time)) >>= peek 
    --putStrLn ("mpv_wait_event: " ++ (show (event_id event)))
    return $ (MS.event_id event) == MS.mpvEventShutdown 

seekAction :: EL.EventLoop -> MLM ()
seekAction loop =
  do
    env <- ask
    lift $ putStrLn $ "seeking to " ++ (show (startTime loop))
    lift $ M.mpvSetPropertyDouble (ctx env) "playback-time" (realToFrac (startTime loop))
    lift $ putStrLn $ "done seeking to " ++ (show (startTime loop))
    return ()

playAction :: EL.EventLoop -> MLM ()
playAction loop =
  do
    setSpeed (EL.speed (val loop))
    setSids (EL.sids (val loop))



createInitialMpvState loops =
  EL.createInitialELState loops
                        defaultNoSrtAction
                        readTimeAction
                        readSpeedAction
                        waitAction
                        seekAction
                        playAction



