--handles controlling the mpv library on behalf of the event loops
module MpvLoops (createInitialMpvState,
                 MLEnv(..), MLM
                ) where

import MpvFFI
import qualified MpvStructs as MS
import Foreign (Ptr,peek,castPtr)
import Foreign.C.String (peekCAString)
import qualified EventLoop as EL
import Control.Monad.Reader
import Loops
import Text.Printf(printf)
import Control.Monad.Trans.Either (EitherT(..),left,right,hoistEither)
data MLEnv = MLEnv { ctx :: Ctx,
                       defaultNoSrtSpeed :: Double
                     }

type MLM = ReaderT MLEnv MFM


setSpeed :: Double -> MLM ()
setSpeed speed =
  do 
    liftIO $ putStrLn $ "setSpeed: "++(show speed)
    env <- ask
    lift $ mpvSetPropertyDouble (ctx env) "speed" (realToFrac speed)
    liftIO $ putStrLn $ "done setSpeed: "++(show speed)
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
    liftIO $ putStrLn $ "setSids " ++ (show (sid,ssid))
    lift $ do
                mpvSetPropertyString (ctx env) "sid" sid
                mpvSetPropertyString (ctx env) "secondary-sid" ssid
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
    time <- (lift $ mpvGetPropertyDouble (ctx env) name)
--    lift $ putStrLn $ "done readDouble: "++name
    return $ realToFrac <$> (eitherToMaybe time)

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right x) = Just x
    
readTimeAction :: MLM (Maybe Double)
readTimeAction = readDouble "playback-time"

readSpeedAction :: MLM (Maybe Double)
readSpeedAction = readDouble "speed"

waitAction :: Double -> MLM EL.ELWaitEvent
waitAction time =
  do
    env <- ask
    event <- lift $ (mpvWaitEvent (ctx env) (realToFrac time)) >>= lift . peek 
    liftIO $ putStrLn ("mpv_wait_event: " ++ (show (event_id event)))

    --TODO FIXME HACK
    runEitherT printTracksInfo
    -- time <- readTimeAction
    -- liftIO $ putStrLn ("time is "++show time)
    case (MS.event_id event) of
      m | m == MS.mpvEventShutdown -> return EL.ELWShutdown
      --m | m == MS.mpvEventPlaybackRestart -> return EL.ELWSeekFinished
      m | m == MS.mpvEventSeek -> return EL.ELWSeekFinished
      m | m == MS.mpvEventPropertyChange ->
        do
          eventProperty <- liftIO $ peek (castPtr (MS.edata event))
          epName <- liftIO $ peekCAString (MS.name eventProperty)
          case epName of
                "sub-delay" ->
                   do
                     amt <- liftIO (peek (castPtr (MS.pdata eventProperty)))
                     return $ EL.ELWSubDelayChanged amt
                _ -> return EL.ELWOther
               
      _ -> return EL.ELWOther

printTracksInfo :: EitherT String MLM ()
printTracksInfo =
  do
    env <- lift $ ask
    et <- (lift $ lift $ mpvGetPropertyInt (ctx env) "track-list/count")
    tracks <- hoistEitherAdjustError et
    liftIO $ putStrLn $ "Tracks count is " ++ (show tracks)
    mapM printTrackInfo  [0..(tracks-1)]

    return ()
  where
    hoistEitherAdjustError :: Monad m => Either MS.MpvError a -> EitherT String m a
    hoistEitherAdjustError (Left e) = left $ show e
    hoistEitherAdjustError (Right a) = return a
    printTrackInfo :: Int -> EitherT String MLM ()
    printTrackInfo trk =
      do
        env <- lift $ ask
        trackType <-
          (lift $ lift $ mpvGetPropertyString (ctx env)
          $ "track-list/"++(show trk)++"/type") >>= hoistEitherAdjustError
        trackId <-
          (lift $ lift $ mpvGetPropertyInt (ctx env)
          $ "track-list/"++(show trk)++"/id") >>= hoistEitherAdjustError
        trackExternal <-
          (lift $ lift $ mpvGetPropertyBool (ctx env)
          $ "track-list/"++(show trk)++"/external") >>= hoistEitherAdjustError
          
        trackFN <- if trackType == "sub" && trackExternal
          then (lift $ lift $ mpvGetPropertyString (ctx env)
                 $ "track-list/"++(show trk)++"/external-filename") >>= hoistEitherAdjustError
          else return ""
        liftIO $ putStrLn $ (printf "Track %d: type %s id: %d ext: %s fn: %s" trk trackType trackId (show trackExternal) trackFN)
        
        



seekAction :: Double -> MLM ()
seekAction startTime =
  do
    env <- ask
    liftIO $ putStrLn $ "seeking to " ++ (show startTime)
    lift $ mpvSetPropertyDouble (ctx env) "playback-time" (realToFrac startTime)
    liftIO $ putStrLn $ "done seeking to " ++ (show startTime)
    return ()

playAction :: EL.EventLoop -> MLM ()
playAction loop =
  do
    setSpeed (EL.speed (val loop))
    setSids (EL.sids (val loop))

elLog :: String -> MLM()
elLog msg = liftIO $ putStrLn msg


createInitialMpvState :: [EL.EventLoop] -> EL.ELState MLM
createInitialMpvState loops =
  EL.createInitialELState loops
                        defaultNoSrtAction
                        readTimeAction
                        readSpeedAction
                        waitAction
                        seekAction
                        playAction
                        elLog



