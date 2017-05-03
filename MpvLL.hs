module Main where

import System.Environment (getArgs)
import Core

main :: IO ()
main =
  do
    argsStr <- getArgs
    commandLine argsStr
