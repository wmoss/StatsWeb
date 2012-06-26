import Network.StatsWeb (initStats, runStats, addCounter)

import Control.Monad (forever)
import Control.Concurrent (threadDelay, forkIO)



main = do
       -- initialize a TVar protected Text-Int Map
       stats <- initStats
       -- increment the counter and give it a name
       counter <- addCounter stats "test1"
       -- in a separate thread, continually call the counter action
       -- make sure "test1" is only being called once
       -- make a ghc object every second
       forkIO $ tick counter
       runStats stats 3344
       



tick counter = forever $ do
    counter
    threadDelay 1000000
