module EventLoopTest where

import EventLoop

loops1 = []

defaultNoSrtAction1 = putStrLn "default nosrt action"
readTimeAction1 = putStrLn "readTimeAction1" >> (return $ Just 0.0)
waitAction1 time = (putStrLn $ "waitAction1 "++(show time)) >> return True
seekAction1 loop = putStrLn $ "seekAction1 "++(show loop)
playAction1 loop = putStrLn $ "playAction1 "++(show loop)

st1 = createInitialMpvState loops1 defaultNoSrtAction1 readTimeAction1 waitAction1 seekAction1
                            playAction1
