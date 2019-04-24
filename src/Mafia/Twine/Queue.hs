{-# LANGUAGE NoImplicitPrelude #-}
module Mafia.Twine.Queue (
    Queue
  , newQueue
  , readQueue
  , tryReadQueue
  , writeQueue
  , isQueueEmpty
  ) where

import           Control.Concurrent.STM.TBQueue (TBQueue, newTBQueue, tryReadTBQueue
                                                , readTBQueue, writeTBQueue, isEmptyTBQueue)

import           GHC.Conc (atomically)

import           Mafia.P

import           System.IO

newtype Queue a =
  Queue {
      queue :: TBQueue a
    }

newQueue :: Int -> IO (Queue a)
newQueue i =
  atomically $ Queue <$> newTBQueue (fromIntegral i)

readQueue :: Queue a -> IO a
readQueue =
  atomically . readTBQueue . queue

tryReadQueue :: Queue a -> IO (Maybe a)
tryReadQueue =
  atomically . tryReadTBQueue . queue

writeQueue :: Queue a -> a -> IO ()
writeQueue q =
  atomically . writeTBQueue (queue q)

isQueueEmpty :: Queue a -> IO Bool
isQueueEmpty =
  atomically . isEmptyTBQueue . queue
