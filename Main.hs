module Main where

import Init
import System.Environment
import MpvFFI
import Data.List.Split (splitOn)
import Data.List (isPrefixOf,elemIndex)
import Text.Read (readMaybe)
import Control.Exception.Base (Exception,throwIO)
import Control.Monad.State (StateT, runStateT, get, modify,lift)
import Data.Either (rights)
import SrtFile (loadSrtFile, Srt)
import qualified SrtFile as SF
import Text.Printf (printf)

import Foreign (Ptr,peek)
import MpvStructs
import EventLoop
import Util
import Loops (sortLoopsForPlay)
import Control.Monad.Reader (runReaderT,liftIO)
import MpvLoops 

data MyException = MyException String deriving (Show)
instance Exception MyException 



runit :: Conf -> ELState MLM -> MFM ()         
runit conf mpvState = 
  do
    ctx <- mpvCreate
    liftIO $ putStrLn "created context"
    mpvSetOptionString ctx "input-default-bindings" "yes"
    mpvSetOptionString ctx "input-vo-keyboard" "yes"
    mpvSetOptionFlag ctx "osc" 1
    liftIO $ putStrLn $ "set flags for subfiles "++(show (subfiles conf))
    liftIO $ putStrLn "set options"
    setupMpvFlags ctx (flags (mpvArgs conf))
    setupMpvOptions ctx (opts (mpvArgs conf))
    setMultipleSubfiles ctx (subfiles conf)
    mpvInitialize ctx
    liftIO $ putStrLn "initialized"
    --TODO if file doesn't exist, doesn't report an error
    loadFiles ctx (singleArgs (mpvArgs conf))
    liftIO $ putStrLn "loaded files"
    runReaderT (eventLoop mpvState) (MLEnv ctx 1.0) 
    liftIO $ putStrLn "finished event loop"
    mpvTerminateDestroy ctx -- this should be in some sort of failsafe (like java finally)
    return ()

main :: IO ()
main =
  do
    argsStr <- getArgs
    c <- case (parseArgs argsStr) of
                 Left s -> throwIO $ MyException $ "Error: " ++ s
                 Right c -> return c
    srtArrays <- doMonadOnList (subfiles c) loadSrtFileAndPrintErrors
    let loopArrays = createLoopArrays srtArrays (tracks c)
        mpvState = createInitialMpvState (sortLoopsForPlay loopArrays)
    runReaderT (runit c mpvState) (MpvFFIEnv errorFunc)
  where
    errorFunc call mpvError = lift $ putStrLn $
      printf "Error: call %s, status %s" (show call) (show mpvError)
    loadSrtFileAndPrintErrors :: String -> IO [Srt]
    loadSrtFileAndPrintErrors f =
      do
        (errorsOrSrts) <- loadSrtFile f
        printErrors errorsOrSrts
        return $ rights errorsOrSrts

    printErrors :: [Either [String] Srt] ->  IO ()
    printErrors array =
      do
        runStateT (doMonadOnList array printError) 1
        return ()
        
    printError ::(Either [String] Srt) -> StateT Int IO ()
    printError (Left error) =
                     do
                        line <- get
                        lift $ putStrLn
                          (printf "Error processing sub at line %d, skipping:\n%s" line
                                  (foldr (\x -> \y -> x ++ "\n" ++ y) "" error))
                        let len = (length error)
                        modify (+ len)
    printError (Right srt) =
      do
        --lift $ putStrLn $ show srt
        modify (+ (SF.lines srt))
    
doMonadOnList :: Monad m => [a] -> (a -> m b) -> (m [b])
doMonadOnList [] _ = return []
doMonadOnList (a : as) f =
   do
     b <- (f a)
     bs <- doMonadOnList as f
     return (b : bs)
                                                        
