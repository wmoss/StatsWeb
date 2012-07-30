module Network.StatsWeb (
    Stats,
    initStats,
    runStats,
    addCounter,
    incCounter,
    setCounter
    ) where

import Control.Monad.Trans (liftIO)
import Data.Aeson ((.=), object)
import Data.Aeson.Encode (fromValue)
import Web.Scotty (scotty, get, html)
import qualified Data.Text as T
import Data.Text.Lazy.Builder (toLazyText)
import qualified Data.Map as M
import Control.Applicative
import Control.Arrow

import System.IO (hPutStrLn, stderr)

import GHC.Conc (TVar, newTVar, newTVarIO, readTVar, readTVarIO, writeTVar, atomically)
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

{-
(.=) :: ToJSON a => Text -> a -> Pair
name .= value = (name, toJSON value)
-}
runStats :: Stats -> Int -> IO ()
runStats stats port = do
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
                          , "gc.bytes.copied.par.avg" .= parAvgBytesCopied gcStats
                          , "gc.bytes.copied.par.max" .= parMaxBytesCopied gcStats]

            counters <- liftIO $ map (uncurry (.=)) <$> (flattenStats $ tvstats stats)
            html $ toLazyText $ fromValue $ object $ map addPrefix $ garbage ++ counters
  where
    addPrefix = first . T.append $ prefix stats

initStats :: T.Text -> IO Stats
initStats pfx = Stats pfx <$> newTVarIO M.empty

modifyTVarIO :: (a -> a) -> TVar a -> IO ()
modifyTVarIO f tv = atomically $ do
    v <- readTVar tv
    writeTVar tv $! f v

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
    Nothing -> hPutStrLn stderr $ "counter " ++ (T.unpack name) ++ " not added to Stats Map"

incCounter :: T.Text -> Stats -> IO ()
incCounter name stats =
    modifyCounter stats name $ modifyTVarIO (+1)

setCounter :: T.Text -> Int -> Stats -> IO ()
setCounter name val stats =
    modifyCounter stats name $ modifyTVarIO $ \_ -> val
