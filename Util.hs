module Util where

--TODO is there a generic method to do this? reminds me of fold... is it related?
recurseMonad :: (Monad b) => [a] -> (a -> b x) -> b ()
recurseMonad [] _ = return ()
recurseMonad (a : as) f = (f a) >> (recurseMonad as f)

