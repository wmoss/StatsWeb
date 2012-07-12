import Network.StatsWeb (initStats, runStats,
                         addCounter, incCounter)

import Control.Monad (forever)
import Control.Concurrent (threadDelay, forkIO)


main = do
       -- initialize a TVar protected Text-Int Map
       stats <- initStats "statsweb.test."
       -- increment the counter and give it a name
       addCounter stats "test1"
       -- in a separate thread, continually call the counter action
       -- make sure "test1" is only being called once
       -- make a ghc object every second
       forkIO $ tick stats
       runStats stats 3344

tick stats = forever $ do
    incCounter "test1" stats
    threadDelay 1000000
