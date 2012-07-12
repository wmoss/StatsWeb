module Bump.StatsWeb.Network.StatsWeb (
    Stats,
    initStats,
    runStats,
    addCounter,
    incCounter,
    setCounter
    ) where

import Control.Monad (forever, void)
import Control.Monad.Trans (liftIO)
import Control.Concurrent (threadDelay, forkIO)

import Data.Maybe (fromJust)
import Data.Aeson (Value(..), (.=), object, encode)
import Data.Aeson.Encode (fromValue)
import Web.Scotty (scotty, get, html)
import qualified Data.Text as T
import Data.Text.Lazy.Builder (toLazyText)
import qualified Data.Map as M
import Control.Applicative

import System.IO (hPutStrLn, stderr)

import GHC.Conc (TVar, newTVar, newTVarIO, readTVar, writeTVar, atomically)
import GHC.Stats


type Stats = TVar (M.Map T.Text (TVar Int))

flattenStats :: Stats -> IO [(T.Text, Int)]
flattenStats tvstats = atomically $ do
    stats <- readTVar tvstats
    counts <- mapM readTVar $ M.elems stats
    return $ zip (M.keys stats) counts

{-
(.=) :: ToJSON a => Text -> a -> Pair
name .= value = (name, toJSON value)
-}
runStats :: Stats -> Int -> IO ()
runStats tvstats port = do
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

            stats <- liftIO $ map (uncurry (.=)) <$> flattenStats tvstats
            html $ toLazyText $ fromValue $ object (garbage ++ stats)

initStats :: IO Stats
initStats = newTVarIO M.empty

tick :: TVar Int -> IO ()
tick counter = atomically $ do
    v <- readTVar counter
    writeTVar counter $ v + 1

set :: Int -> TVar Int -> IO ()
set val counter = atomically $ do
  writeTVar counter $ val

addCounter :: Stats -> T.Text -> IO ()
addCounter stats name = atomically $ do
  counter <- newTVar 0
  s <- readTVar stats
  writeTVar stats $ M.insert name counter s

getCounter :: Stats -> T.Text -> (TVar Int -> IO ()) -> IO (IO ())
getCounter stats name action = atomically $ do
  s <- readTVar stats
  let counter = M.lookup name s
  case counter of
    Just c -> return $ action c
    Nothing -> return $ hPutStrLn stderr $ "counter " ++ (T.unpack name) ++ " not added to Stats Map"
incCounter :: T.Text -> Stats -> IO ()
incCounter name stats = do
  counter <- getCounter stats name tick
  counter

setCounter :: T.Text -> Int -> Stats -> IO ()
setCounter name val stats = do
  counter <- getCounter stats name (set val)
  counter


