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
    ctx <- mpvCreate
    putStrLn "created context"
    mpvSetOptionString ctx "input-default-bindings" "yes"
    mpvSetOptionString ctx "input-vo-keyboard" "yes"
    mpvSetOptionFlag ctx "osc" 1
    putStrLn $ "set flags for subfiles "++(show (subfiles conf))
    putStrLn "set options"
    setupMpvFlags ctx (flags (mpvArgs conf))
    setupMpvOptions ctx (opts (mpvArgs conf))
    setMultipleSubfiles ctx (subfiles conf)
    mpvInitialize ctx
    putStrLn "initialized"
    --TODO if file doesn't exist, doesn't report an error
    loadFiles ctx (singleArgs (mpvArgs conf))
    putStrLn "loaded files"
    runReaderT (eventLoop mpvState) (ML.MLEnv ctx 1.0) 
    putStrLn "finished event loop"
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
                                                        
