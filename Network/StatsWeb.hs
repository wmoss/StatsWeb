module Network.StatsWeb (
    Stats,
    initStats,
    runStats,
    addCounter,
    showCounter,
    incCounter,
    incCounterBy,
    setCounter
    ) where

import Control.Monad.Trans (liftIO)
import Data.Aeson ((.=), object, Value)
import Data.Aeson.Encode (fromValue)
import Web.Scotty (scotty, get, html)
import qualified Data.Text as T
import Data.Text.Format
import Data.Text.Lazy.Builder (toLazyText)
import qualified Data.Text.Lazy as TL
import qualified Data.Map as M
import Control.Applicative
import Control.Arrow

import System.IO (hPutStrLn, stderr)

import GHC.Conc (TVar, newTVar, newTVarIO, readTVar, readTVarIO, writeTVar, atomically)
import Control.Concurrent.STM.TVar (modifyTVar')
import GHC.Stats


data Stats = Stats {
    prefix   :: T.Text,
    tvstats  :: TVar (M.Map T.Text (TVar Int))
    }

flattenStats :: TVar (M.Map T.Text (TVar Int)) -> IO [(T.Text, Int)]
flattenStats stats = atomically $ do
    stats' <- readTVar stats
    counts <- mapM readTVar $ M.elems stats'
    return $ zip (M.keys stats') counts

runStats :: Stats -> Int -> IO ()
runStats stats port = runStats' stats port (addPostfix . addPrefix)
  where
    addPrefix = first . T.append $ prefix stats
    addPostfix (t,v) = (T.append t $ TL.toStrict $ format "[port={}]" (Only $ Shown port), v)

runStats' :: Stats -> Int -> ((T.Text,Value) -> (T.Text,Value)) -> IO ()
runStats' stats port modifyMetric = do
    scotty port $ do
        get "/:stats" $ do
            gcStats <- liftIO getGCStats
            let garbage = [ "gc.bytes.allocated" .= bytesAllocated gcStats
                          , "gc.num.gcs" .= numGcs gcStats
                          , "gc.bytes.used.max" .= maxBytesUsed gcStats
                          , "gc.num.byte.usage.samples" .= numByteUsageSamples gcStats
                          , "gc.bytes.used.cum" .= cumulativeBytesUsed gcStats
                          , "gc.bytes.copied" .= bytesCopied gcStats
                          , "gc.bytes.used.current" .= currentBytesUsed gcStats
                          , "gc.bytes.slop.current" .= currentBytesSlop gcStats
                          , "gc.bytes.slop.max" .= maxBytesSlop gcStats
                          , "gc.peak.megabytes.allocated" .= peakMegabytesAllocated gcStats
                          , "gc.seconds.mutator.cpu" .= mutatorCpuSeconds gcStats
                          , "gc.seconds.mutator.wall" .= mutatorWallSeconds gcStats
                          , "gc.seconds.gc.cpu" .= gcCpuSeconds gcStats
                          , "gc.seconds.gc.wall" .= gcWallSeconds gcStats
                          , "gc.seconds.cpu" .= cpuSeconds gcStats
                          , "gc.seconds.wall" .= wallSeconds gcStats
                          , "gc.bytes.copied.par.max" .= parMaxBytesCopied gcStats]

            counters <- liftIO $ map (uncurry (.=)) <$> (flattenStats $ tvstats stats)
            html $ toLazyText $ fromValue $ object $ map modifyMetric $ garbage ++ counters

initStats :: T.Text -> IO Stats
initStats pfx = Stats pfx <$> newTVarIO M.empty



modifyTVarIO :: (a -> a) -> TVar a -> IO ()
modifyTVarIO =
    atomically .* flip modifyTVar'
  where
    (.*) = (.) . (.)

addCounter :: Stats -> T.Text -> IO ()
addCounter stats name = atomically $ do
    counter <- newTVar 0
    s <- readTVar $ tvstats stats
    writeTVar (tvstats stats) $ M.insert name counter s

modifyCounter :: Stats -> T.Text -> (TVar Int -> IO ()) -> IO ()
modifyCounter stats name action = do
    counter <- M.lookup name <$> (readTVarIO $ tvstats stats)
    case counter of
        Just c -> action c
        Nothing -> addCounter stats name >> modifyCounter stats name action

showCounter :: T.Text -> Stats -> IO (Maybe Int)
showCounter name stats = do
    counter <- M.lookup name <$> (readTVarIO $ tvstats stats)
    case counter of
        Just c -> readTVarIO c >>= (\x -> return $ Just x)
        Nothing -> return Nothing

incCounter :: T.Text -> Stats -> IO ()
incCounter name stats =
    modifyCounter stats name $ modifyTVarIO (+1)

incCounterBy :: Int -> T.Text -> Stats -> IO ()
incCounterBy by name stats =
    modifyCounter stats name $ modifyTVarIO (+by)

setCounter :: T.Text -> Int -> Stats -> IO ()
setCounter name val stats =
    modifyCounter stats name $ modifyTVarIO $ \_ -> val
