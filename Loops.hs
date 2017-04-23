module Loops where

import Data.List
import Data.Ord

data Loop a = Loop { val :: a, startTime :: Double, endTime :: Double } deriving (Show)

instance Eq (Loop a) where
  a == b = (startTime a) == (startTime b) &&
         (endTime a) == (endTime b)
instance Ord (Loop a) where
   compare a b = compare (startTime a) (startTime b)


--orders loops with a track number
--the rules are:
-- 1. loops in a single track are ordered by startTime (and shouldn't overlap)
-- 2. if a loop in an early track has a start time before the end time of a loop in a later
--    track, the loop in the earlier track goes first
loopOrdering :: ( (Loop a), Int ) ->  ( (Loop a), Int ) ->  Ordering
loopOrdering (a,ta) (b,tb) | ta == tb = compare (startTime a) (startTime b)
                           | ta < tb = if (startTime a) < (endTime b) then LT else GT
                           | ta > tb = if (endTime a) <= (startTime b) then LT else GT


--flattens a list of lists of values to a single flat list with a number indicating the index
-- of the inner list in the outer list the element was in
flattenTracks :: [[x]] -> [(x,Int)]
flattenTracks l = ft1 0 l
  where
    ft1 :: Int -> [[x]] -> [(x,Int)]
    ft1 ct [] = []
    ft1 ct ([] : xs) = ft1 (ct + 1) xs
    ft1 ct ((x : xs) : ys) = (x, ct) : ft1 ct (xs : ys)
    

sortLoopsForPlay :: [[Loop a]] -> [Loop a]
sortLoopsForPlay tl = fmap (\(x,_) -> x) (sortBy loopOrdering (flattenTracks tl))




-- --creates a list of actions to perform each with a time value to perform them at
-- process :: Int -> [[Loop a]] -> [(a , Int)]
-- process startTime tracks = processWithState startTime (cutTracksToStartTime tracks startTime)
--   where
--     processWithState _ [] = [] --no more loops, no more actions
--     processWithState t [ [] : xs ] = processWithState t xs --first track has no more elems, so remove it
--     processWithState t [ (l : ls) : [] ] = createActions l (processWithState (endTime l) (ls : []))
--     processWithState t [ (l : ls) :  ] = createActions l (processWithState (endTime l) (ls : []))
      
