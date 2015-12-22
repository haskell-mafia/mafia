{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Mafia.Init
  ( initialize
  , determineCacheUpdates
  , putUpdateReason
  , runCacheUnregister
  ) where

import           Control.Exception (IOException)
import           Control.Monad.Catch (MonadCatch(..), handle)
import           Control.Monad.IO.Class (MonadIO(..))

import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Time (getCurrentTime)

import           Mafia.Cabal
import           Mafia.Error
import           Mafia.Git
import           Mafia.IO
import           Mafia.Path
import           Mafia.Process
import           Mafia.Submodule

import           P

import           System.IO (IO, stderr)

import           X.Control.Monad.Trans.Either (EitherT, hoistEither, runEitherT, firstEitherT)


initialize :: EitherT MafiaError IO ()
initialize = do
  updates <- determineCacheUpdates
  hasDist <- liftIO $ doesDirectoryExist "dist"
  when (updates /= [] || not hasDist) $ do
    -- we want to know up front why we're doing an install/configure
    let sortedUpdates = List.sort updates
    mapM_ putUpdateReason sortedUpdates
    mapM_ runCacheUnregister sortedUpdates

    firstEitherT MafiaCabalError $ cabal_ "install"
      [ "-j"
      , "--only-dependencies"
      , "--force-reinstalls"
      , "--enable-tests"
      , "--enable-benchmarks"
      , "--reorder-goals"
      , "--max-backjumps=-1" ]

    firstEitherT MafiaCabalError $ cabal_ "configure"
      [ "--enable-tests"
      , "--enable-benchmarks" ]

    -- but we don't want to commit the modified .cabal files
    -- until we're done, in case an error occurs
    mapM_ runCacheUpdate sortedUpdates

determineCacheUpdates :: EitherT MafiaError IO [CacheUpdate]
determineCacheUpdates = do
    liftGit initSubmodules
    syncCabalSources

    currentDir  <- getCurrentDirectory
    sandboxSrcs <- Set.toList <$> getSandboxSources
    let allSrcs = currentDir : sandboxSrcs

    srcs     <- mkFileMap . concat <$> mapM findCabal allSrcs
    cacheDir <- getCacheDir
    dsts     <- mkFileMap <$> getDirectoryListing (RecursiveDepth 0) cacheDir

    let mkAdd src = Add src (cacheDir </> takeFileName src)

        fresh = fmap mkAdd  (Map.elems (srcs `Map.difference` dsts))
        stale = fmap Delete (Map.elems (dsts `Map.difference` srcs))

    updates <- sequence (Map.elems (Map.intersectionWith cacheUpdate srcs dsts))

    return (stale <> fresh <> catMaybes updates)

findCabal :: MonadIO m => Directory -> m [Path]
findCabal dir = filter (extension ".cabal")
        `liftM` getDirectoryListing (RecursiveDepth 0) dir

mkFileMap :: [Path] -> Map File Path
mkFileMap = Map.fromList . fmap (\path -> (takeFileName path, path))

putUpdateReason :: (Functor m, MonadIO m) => CacheUpdate -> m ()
putUpdateReason x =
  case x of
    Add src _ -> do
      rel <- fromMaybe src <$> makeRelativeToCurrentDirectory src
      liftIO (T.hPutStrLn stderr ("Cache: Adding " <> rel))

    Update src _ -> do
      rel <- fromMaybe src <$> makeRelativeToCurrentDirectory src
      liftIO (T.hPutStrLn stderr ("Cache: Updating " <> rel))

    Delete file -> do
      liftIO (T.hPutStrLn stderr ("Cache: Removing " <> takeFileName file))

runCacheUpdate :: CacheUpdate -> EitherT MafiaError IO ()
runCacheUpdate x = handleCacheUpdateError x $
  case x of
    Add src dst -> do
      copyFile src dst

    Update src dst -> do
      copyFile src dst

    Delete file -> do
      removeFile file

handleCacheUpdateError :: (Functor m, MonadCatch m)
                       => CacheUpdate
                       -> EitherT MafiaError m a
                       -> EitherT MafiaError m a

handleCacheUpdateError x =
  handle (\(ex :: IOException) -> hoistEither . Left $ MafiaCacheUpdateError x ex)

runCacheUnregister :: MonadIO m => CacheUpdate -> m ()
runCacheUnregister x =
  case x of
    Add src dst -> do
      tryUnregisterPackage src
      tryUnregisterPackage dst
      invalidateCache dst

    Update src dst -> do
      tryUnregisterPackage src
      tryUnregisterPackage dst
      invalidateCache dst

    Delete file -> do
      tryUnregisterPackage file
      invalidateCache file

tryUnregisterPackage :: MonadIO m => File -> m ()
tryUnregisterPackage cabalFile = do
  mpkg <- readPackageName cabalFile
  case mpkg of
    Nothing  -> return ()
    Just pkg -> do
      -- This is only best effort, if unregistering fails it means we've probably
      -- already unregistered the package or it was never registered, no harm is
      -- done because we'll be reinstalling it later if it's required.

      result <- liftIO . runEitherT $ do
        Hush <- cabal "sandbox" ["hc-pkg", "--", "unregister", "--force", pkg]
        return ()

      case result of
        Left  _ -> return ()
        Right _ -> liftIO (T.putStrLn ("Sandbox: Unregistered " <> pkg))

invalidateCache :: MonadIO m => File -> m ()
invalidateCache cabalFile = do
  mtxt <- readUtf8 cabalFile
  case mtxt of
    Nothing  -> return ()
    Just txt -> do
      time <- liftIO getCurrentTime
      let header = "-- " <> T.pack (show time) <> "\n"
      writeUtf8 cabalFile (header <> txt)

cacheUpdate :: (Functor m, MonadIO m) => File -> File -> m (Maybe CacheUpdate)
cacheUpdate src dst = do
  src_missing <- not <$> doesFileExist src
  dst_missing <- not <$> doesFileExist dst
  if | src_missing && dst_missing -> return (Nothing)
     | src_missing                -> return (Just (Delete dst))
     | dst_missing                -> return (Just (Update src dst))
     | otherwise                  -> cacheUpdateDiff src dst

cacheUpdateDiff :: MonadIO m => File -> File -> m (Maybe CacheUpdate)
cacheUpdateDiff src dst = do
  src_bytes <- readBytes src
  dst_bytes <- readBytes dst
  if | src_bytes == dst_bytes -> return (Nothing)
     | otherwise              -> return (Just (Update src dst))

getCacheDir :: EitherT MafiaError IO Directory
getCacheDir = do
  cacheDir <- (</> "mafia") <$> firstEitherT MafiaCabalError initSandbox
  createDirectoryIfMissing False cacheDir
  return cacheDir
