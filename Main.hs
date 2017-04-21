module Main where

import Init
import System.Environment
import MpvLL
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
import Control.Monad.Reader (runReaderT)
import qualified MpvLoops as ML

data MyException = MyException String deriving (Show)
instance Exception MyException 


     
runit conf mpvState = 
  do
    ctx <- mpv_create
    putStrLn "created context"
    mpv_set_option_string ctx "input-default-bindings" "yes"
    mpv_set_option_string ctx "input-vo-keyboard" "yes"
    mpv_set_option_flag ctx "osc" 1
    putStrLn "set flags for subfiles"
    recurseMonad (subfiles conf) (\sf -> mpv_set_option_string ctx "sub-file" sf )
    putStrLn "set options"
    setupMpvFlags ctx (flags (mpvArgs conf))
    setupMpvOptions ctx (opts (mpvArgs conf))
    mpv_initialize ctx
    putStrLn "initialized"
    --TODO if file doesn't exist, doesn't report an error
    loadFiles ctx (singleArgs (mpvArgs conf))
    putStrLn "loaded files"
    runReaderT (event_loop mpvState) (ML.MLEnv ctx 1.0) 
--    event_loop ctx undefined
    putStrLn "finished event loop"
    mpv_terminate_destroy ctx -- this should be in some sort of failsafe (like java finally)
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
        mpvState = ML.createInitialMpvState (sortLoopsForPlay loopArrays)
    runit c mpvState
  where
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
                                  (foldr (++) "" error))
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
                                                        
