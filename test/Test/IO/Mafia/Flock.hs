{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.IO.Mafia.Flock where

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.MVar (newEmptyMVar, readMVar, putMVar)
import           Control.Concurrent.Async (async, mapConcurrently, wait)
import           Control.Monad.Trans.Either (runEitherT)

import           Data.IORef (newIORef, modifyIORef', readIORef)
import qualified Data.Text as T

import           Test.Mafia.IO (testIO)

import           Mafia.Flock
import           Mafia.Path
import           Mafia.P

import           System.IO (IO)
import qualified System.IO.Temp as Temp

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

prop_flock :: Property
prop_flock =
  withTempDirectory $ \dir -> do
    pin <- newEmptyMVar
    ref <- newIORef (0 :: Int)

    let
      loop :: Int -> IO ()
      loop _ = do
        readMVar pin
        _ <- runEitherT . withFileLock (dir <> "/foo") (pure ()) $
          liftIO $ modifyIORef' ref (+1)
        pure ()

    x <- async $ mapConcurrently loop ([0..999] :: [Int])
    threadDelay 50000 -- nick's fault
    putMVar pin ()
    _ <- wait x

    n <- readIORef ref

    pure $
      n === 1000

withTempDirectory :: (Directory -> IO Property) -> Property
withTempDirectory io =
  testIO . Temp.withSystemTempDirectory "flock.test" $ io . T.pack

return []
tests :: IO Bool
tests =
  $quickCheckAll
