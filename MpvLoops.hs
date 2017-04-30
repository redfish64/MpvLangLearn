--handles controlling the mpv library on behalf of the event loops
module MpvLoops (createInitialMpvState,
                 MLEnv(..), MLM, MLState(..)
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
import Control.Monad.Trans.State (StateT(..),get,put)
data MLEnv = MLEnv { ctx :: Ctx,
                     defaultNoSrtSpeed :: Double,
                     subTitleFilenames :: [String]
                     }
data MLState = MLState {
  subTrackMapping :: Maybe [Int] -- maps srt sub tracks that we know about to their corresponding
                     -- ids in the video file. Since the video file can contain other sub
                     -- tracks, this can affect which subtitle gets displayed for which id.
                       }

type MLM = ReaderT MLEnv (StateT MLState MFM)


setSpeed :: Double -> MLM ()
setSpeed speed =
  do 
    liftIO $ putStrLn $ "setSpeed: "++(show speed)
    env <- ask
    lift $ lift $ mpvSetPropertyDouble (ctx env) "speed" (realToFrac speed)
    liftIO $ putStrLn $ "done setSpeed: "++(show speed)
    return ()

setSids :: [Int] -> MLM ()
setSids sids =
  do
    env <- ask
    maybeMapping <- getOrCreateSubTrackMapping
    case maybeMapping of
      Nothing -> return ()
      (Just mapping) ->
        do
          let (sid,ssid) = case sids of
                           [] -> ("no","no")
                           --TODO, use int based set option
                           x : [] -> (show (mapping !! (x-1)),"no")
                           x1 : x2 : _ -> (show (mapping !! (x1-1)),
                                           show (mapping !! (x2-1)))
          liftIO $ putStrLn $ "setSids " ++ show (sid,ssid)
          lift $ lift $ do
            mpvSetPropertyString (ctx env) "sid" sid
            mpvSetPropertyString (ctx env) "secondary-sid" ssid
          return ()

--if mapping is already in the state, uses it. Otherwise attempts to get it from
--the libmpv system
getOrCreateSubTrackMapping :: MLM (Maybe [Int])
getOrCreateSubTrackMapping =
  do
    state <- lift $ get
    case (subTrackMapping state) of
      mm@(Just _) -> return mm
      Nothing ->
        do
          mapping <- createSubTrackMapping
          lift $ put (state { subTrackMapping = mapping })
          return mapping


data TrackInfo =
  TrackInfo { tiType :: String, tiId :: Int, tiExtFile :: Maybe String } deriving (Show)

getExternalFileToSidMapping :: MLM (Maybe [(String,Int)])
getExternalFileToSidMapping = do
  env <- ask
  eitherTracks <- 
    runEitherT (do
                   c <- ((lift $ lift $ lift $ mpvGetPropertyInt (ctx env) "track-list/count")
                          >>= hoistEither)
                   if c == 0 then left $ generalMpvError else return ()
                   tis <- doit [0..(c-1)]
                   let ftis = filter (\ti -> (tiExtFile ti) /= Nothing) tis
                   -- liftIO $ putStrLn (printf "tis is: %s ftis is %s" (show tis) (show ftis))
                   return $ map convertTi ftis
               )
  return (
    case eitherTracks of
      Left _ -> Nothing
      (Right res) -> Just res
    )
    
  where
    doit :: [Int] -> EitherT MS.MpvError MLM [TrackInfo]
    doit [] = return []
    doit (t : ts) =
      do
        ti <- getTrackInfo t
        (doit ts) >>= (return . (ti : ))
    convertTi :: TrackInfo -> (String,Int)
    convertTi (TrackInfo _ tiId (Just tiExtFile)) =
      (tiExtFile,tiId)
      


getTrackInfo :: Int -> EitherT MS.MpvError MLM TrackInfo
getTrackInfo tid =
  let stid = (show tid)
  in
    do
      env <- lift $ ask
      trackType <- doMpvFfiAction $
        mpvGetPropertyString (ctx env) $ "track-list/"++stid++"/type"
      trackId <- doMpvFfiAction $
        mpvGetPropertyInt (ctx env) $ "track-list/"++stid++"/id"
      trackExternal <- doMpvFfiAction $
        mpvGetPropertyBool (ctx env) $ "track-list/"++stid++"/external"
      trackFN <- if trackType == "sub" && trackExternal
                 then (doMpvFfiAction $
                       mpvGetPropertyString (ctx env)
                       $ "track-list/"++stid++"/external-filename") >>= return . Just
                 else return Nothing
      return $ TrackInfo trackType trackId trackFN
  where
    doMpvFfiAction :: MFM (Either MS.MpvError x) -> EitherT MS.MpvError MLM x
    doMpvFfiAction action =
      do
        (lift $ lift $ lift $ action) >>= hoistEither

    

createSubTrackMapping :: MLM (Maybe [Int])
createSubTrackMapping =
  do
    env <- ask
    let fns = (subTitleFilenames env)
    mEfToSid <- getExternalFileToSidMapping
    -- liftIO $ putStrLn (printf "fns: %s, mEfToSid: %s" (show fns) (show mEfToSid))
    case mEfToSid of
      Nothing -> return Nothing
      Just efToSid -> return $ Just $ doit efToSid fns
  where
    doit :: [(String,Int)] -> [String] -> [Int]
    doit efToSid [] = []
    doit efToSid (fn : fns) =
      case lookup fn efToSid of
        Just sid -> sid : (doit efToSid fns)
        Nothing -> undefined --1 : (doit efToSid fns) --TODO libmpv seems to always return the exact filenames, so this should never be called.. If this one day fails
        --   it's kind of a big deal, so we just let it bail out

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
    time <- (lift $ lift $ mpvGetPropertyDouble (ctx env) name)
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
    event <- lift $ lift $ (mpvWaitEvent (ctx env) (realToFrac time)) >>= lift . peek 
    liftIO $ putStrLn ("mpv_wait_event: " ++ (show (event_id event)))

    -- runEitherT printTracksInfo
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
    et <- (lift $ lift $ lift $ mpvGetPropertyInt (ctx env) "track-list/count")
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
          (lift $ lift $ lift $ mpvGetPropertyString (ctx env)
          $ "track-list/"++(show trk)++"/type") >>= hoistEitherAdjustError
        trackId <-
          (lift $ lift $ lift $ mpvGetPropertyInt (ctx env)
          $ "track-list/"++(show trk)++"/id") >>= hoistEitherAdjustError
        trackExternal <-
          (lift $ lift $ lift $ mpvGetPropertyBool (ctx env)
          $ "track-list/"++(show trk)++"/external") >>= hoistEitherAdjustError
          
        trackFN <- if trackType == "sub" && trackExternal
          then (lift $ lift $ lift $ mpvGetPropertyString (ctx env)
                 $ "track-list/"++(show trk)++"/external-filename") >>= hoistEitherAdjustError
          else return ""
        liftIO $ putStrLn $ (printf "Track %d: type %s id: %d ext: %s fn: %s" trk trackType trackId (show trackExternal) trackFN)
        
        



seekAction :: Double -> MLM ()
seekAction startTime =
  do
    env <- ask
    liftIO $ putStrLn $ "seeking to " ++ (show startTime)
    lift $ lift $ mpvSetPropertyDouble (ctx env) "playback-time" (realToFrac startTime)
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



