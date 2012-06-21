module Network.StatsWeb (
    initStats,
    runStats,
    addCounter
    ) where

import Control.Monad.Trans (liftIO)
import Data.Aeson (Value(..), (.=), object, encode)
import Data.Aeson.Encode (fromValue)
import Web.Scotty (scotty, get, html)
import qualified Data.Text as T
import Data.Text.Lazy.Builder (toLazyText)
import qualified Data.Map as M

import GHC.Conc (TVar, newTVar, newTVarIO, readTVar, writeTVar, atomically)
import GHC.Stats


type Stats = TVar (M.Map T.Text (TVar Int))

flattenStats :: Stats -> IO [(T.Text, Int)]
flattenStats tvstats = atomically $ do
    stats <- readTVar tvstats
    counts <- mapM readTVar $ M.elems stats
    return $ zip (M.keys stats) counts

runStats :: Stats -> Int -> IO ()
runStats tvstats port = do
    scotty port $ do
        get "/:stats" $ do
            gcStats <- liftIO getGCStats
            let gcStatsObject = ["bytesAllocated" .= bytesAllocated gcStats,
                                 "numGcs" .= numGcs gcStats,
                                 "maxBytesUsed" .= maxBytesUsed gcStats,
                                 "numByteUsageSamples" .= numByteUsageSamples gcStats,
                                 "cumulativeBytesUsed" .= cumulativeBytesUsed gcStats,
                                 "bytesCopied" .= bytesCopied gcStats,
                                 "currentBytesUsed" .= currentBytesUsed gcStats,
                                 "currentBytesSlop" .= currentBytesSlop gcStats,
                                 "maxBytesSlop" .= maxBytesSlop gcStats,
                                 "peakMegabytesAllocated" .= peakMegabytesAllocated gcStats,
                                 "mutatorCpuSeconds" .= mutatorCpuSeconds gcStats,
                                 "mutatorWallSeconds" .= mutatorWallSeconds gcStats,
                                 "gcCpuSeconds" .= gcCpuSeconds gcStats,
                                 "gcWallSeconds" .= gcWallSeconds gcStats,
                                 "cpuSeconds" .= cpuSeconds gcStats,
                                 "wallSeconds" .= wallSeconds gcStats,
                                 "parAvgBytesCopied" .= parAvgBytesCopied gcStats,
                                 "parMaxBytesCopied" .= parMaxBytesCopied gcStats]

            stats <- liftIO $ flattenStats tvstats
            let counterObject = foldl (\acc (k, v) -> (k .= v):acc) [] stats

            html $ toLazyText $ fromValue $ object ["gc stats" .= object gcStatsObject,
                                                    "counters" .= object counterObject]

initStats :: IO Stats
initStats = newTVarIO M.empty

tickCounter :: TVar Int -> IO ()
tickCounter counter = atomically $ do
    v <- readTVar counter
    writeTVar counter $ v + 1

addCounter :: Stats -> T.Text -> IO (IO ())
addCounter stats name = do
    fmap tickCounter $ atomically $ do
        counter <- newTVar 0
        s <- readTVar stats
        writeTVar stats $ M.insert name counter s

        return counter
