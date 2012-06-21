import Network.StatsWeb (initStats, runStats, addCounter)

import Control.Monad (forever)
import Control.Concurrent (threadDelay, forkIO)


main = do
       stats <- initStats
       counter <- addCounter stats "test1"
       forkIO $ tick counter
       runStats stats 3344


tick counter = forever $ do
    counter
    threadDelay 1000000
