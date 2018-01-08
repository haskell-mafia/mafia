{-# LANGUAGE NoImplicitPrelude #-}
module Mafia.Flock (
    withFileLock
  ) where

import           Control.Monad.Catch (MonadMask(..))
import           Control.Monad.IO.Class (MonadIO(..))

import           Control.Concurrent.STM (TMVar, newTMVar, tryTakeTMVar, takeTMVar, putTMVar)
import           Control.Concurrent.STM (TVar, newTVarIO, readTVar, writeTVar)
import           Control.Concurrent.STM (atomically)

import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T

import           Mafia.IO
import           Mafia.Path

import           Mafia.P

import           System.FileLock (SharedExclusive(..), FileLock)
import qualified System.FileLock as FileLock
import           System.IO (IO)
import           System.IO.Unsafe (unsafePerformIO)

import           X.Control.Monad.Trans.Either (EitherT, bracketEitherT')

-- | Take a system-wide lock on a file, process safe and thread safe.
withFileLock :: (MonadIO m, MonadMask m) => File -> EitherT x m () -> EitherT x m a -> EitherT x m a
withFileLock path onWait =
  withThreadLock path onWait .
  withProcessLock path onWait

-- | Take a lock on a file, thread safe but not process safe.
withThreadLock :: (MonadIO m, MonadMask m) => File -> EitherT x m () -> EitherT x m a -> EitherT x m a
withThreadLock path onWait =
  bracketEitherT' (threadLock path onWait) threadUnlock . const

threadLock :: (MonadIO m, MonadMask m) => File -> EitherT x m () -> EitherT x m File
threadLock path onWait = do
  lock <- liftIO $ lookupLock path
  m <- liftIO . atomically $ tryTakeTMVar lock
  case m of
    Just () ->
      return path
    Nothing -> do
      onWait
      liftIO . atomically $ takeTMVar lock
      return path

threadUnlock :: (MonadIO m, MonadMask m) => File -> EitherT x m ()
threadUnlock path =
  liftIO $ do
    lock <- lookupLock path
    atomically $ putTMVar lock ()

lookupLock :: File -> IO (TMVar ())
lookupLock path =
  atomically $ do
    locks <- readTVar locksVar
    case Map.lookup path locks of
      Nothing -> do
        lock <- newTMVar ()
        writeTVar locksVar (Map.insert path lock locks)
        pure lock
      Just lock ->
        pure lock

locksVar :: TVar (Map File (TMVar ()))
locksVar =
  unsafePerformIO $ newTVarIO Map.empty
{-# NOINLINE locksVar #-}

-- | Take a lock on a file, process safe but not thread safe.
withProcessLock :: (MonadIO m, MonadMask m) => File -> EitherT x m () -> EitherT x m a -> EitherT x m a
withProcessLock path onWait =
  bracketEitherT' (processLock path onWait) processUnlock . const

processLock :: (MonadIO m, MonadMask m) => File -> EitherT x m () -> EitherT x m FileLock
processLock path onWait = do
  ignoreIO $ createDirectoryIfMissing True (takeDirectory path)
  mlock <- liftIO $ FileLock.tryLockFile (T.unpack path) Exclusive
  case mlock of
    Just lock ->
      return lock
    Nothing -> do
      onWait
      liftIO $ FileLock.lockFile (T.unpack path) Exclusive

processUnlock :: MonadIO m => FileLock -> m ()
processUnlock =
  liftIO . FileLock.unlockFile
