module Main where

import System.Environment
import MpvLL

main :: IO ()
main =
  do
    args <- getArgs
    putStrLn (show args)
    case args of
      [] -> putStrLn "whats the movie filename, man?"
      x : xs -> play_movie x
