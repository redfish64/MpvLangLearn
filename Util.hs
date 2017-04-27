module Util(recurseMonad,maybeDefault,trim) where

import Data.Text(strip,pack,unpack)

--TODO is there a generic method to do this? reminds me of fold... is it related?
recurseMonad :: (Monad b) => [a] -> (a -> b x) -> b ()
recurseMonad [] _ = return ()
recurseMonad (a : as) f = (f a) >> (recurseMonad as f)


maybeDefault :: Maybe m -> m -> m
maybeDefault Nothing def = def
maybeDefault (Just v) _ = v

trim :: String -> String
trim s = (unpack (strip (pack s)))
