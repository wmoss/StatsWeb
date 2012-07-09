module Bump.StatsWeb.Network.StatsWeb (
    Stats,
    initStats,
    runStats,
    addCounter,
    incCounter,
    setCounter,
    maybeCounter
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
            let garbage = ["bump.matchd2.gc.bytes.allocated" .= bytesAllocated gcStats,
                                 "bump.matchd2.gc.num.gcs" .= numGcs gcStats,
                                 "bump.matchd2.gc.bytes.used.max" .= maxBytesUsed gcStats,
                                 "bump.matchd2.gc.num.byte.usage.samples" .= numByteUsageSamples gcStats,
                                 "bump.matchd2.gc.bytes.used.cum" .= cumulativeBytesUsed gcStats,
                                 "bump.matchd2.gc.bytes.copied" .= bytesCopied gcStats,
                                 "bump.matchd2.gc.bytes.used.current" .= currentBytesUsed gcStats,
                                 "bump.matchd2.gc.bytes.slop.current" .= currentBytesSlop gcStats,
                                 "bump.matchd2.gc.bytes.slop.max" .= maxBytesSlop gcStats,
                                 "bump.matchd2.gc.peak.megabytes.allocated" .= peakMegabytesAllocated gcStats,
                                 "bump.matchd2.gc.seconds.mutator.cpu" .= mutatorCpuSeconds gcStats,
                                 "bump.matchd2.gc.seconds.mutator.wall" .= mutatorWallSeconds gcStats,
                                 "bump.matchd2.gc.seconds.gc.cpu" .= gcCpuSeconds gcStats,
                                 "bump.matchd2.gc.seconds.gc.wall" .= gcWallSeconds gcStats,
                                 "bump.matchd2.gc.seconds.cpu" .= cpuSeconds gcStats,
                                 "bump.matchd2.gc.seconds.wall" .= wallSeconds gcStats,
                                 "bump.matchd2.gc.bytes.copied.par.avg" .= parAvgBytesCopied gcStats,
                                 "bump.matchd2.gc.bytes.copied.par.max" .= parMaxBytesCopied gcStats]
                  
            stats <- liftIO $ flattenStats tvstats
            let counters = foldl (\acc (k, v) -> (T.append "bump.matchd2.count." k .= v):acc) [] stats
            
            html $ toLazyText $ fromValue $ object (garbage ++ counters)

                    
initStats :: IO Stats
initStats = newTVarIO M.empty

tick :: TVar Int -> Maybe Int -> IO ()
tick counter Nothing = atomically $ do
    v <- readTVar counter
    writeTVar counter $ v + 1

set :: TVar Int -> Maybe Int -> IO ()
set counter val = atomically $ do
  writeTVar counter $ fromJust val

addCounter :: Stats -> T.Text -> IO ()
addCounter stats name = atomically $ do
  counter <- newTVar 0
  s <- readTVar stats
  writeTVar stats $ M.insert name counter s

getCounter :: Stats -> T.Text -> Maybe Int -> (TVar Int -> Maybe Int -> IO ()) -> IO (IO ())
getCounter stats name val action = atomically $ do
  s <- readTVar stats
  let counter = M.lookup name s  
  case counter of
    Just _ -> return $ action (fromJust counter) val
    Nothing -> return $ hPutStrLn stderr $ "counter " ++ (T.unpack name) ++ " not added to Stats Map"
    
incCounter :: T.Text -> Stats -> IO ()
incCounter name stats = do
  counter <- getCounter stats name Nothing tick
  counter
  
setCounter :: T.Text -> Int -> Stats -> IO ()
setCounter name val stats = do
  counter <- getCounter stats name (Just val) set
  counter

maybeCounter :: (Num a) => (a -> Bool) -> a -> IO () -> IO () 
maybeCounter cond val action
  | cond val = action
  | otherwise = return ()
                
