{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Mafia.IO
  ( -- * Directory Operations
    ListingOptions(..)
  , getDirectoryListing
  , getDirectoryContents
  , createDirectoryIfMissing
  , removeDirectoryRecursive
  , renameDirectory
  , setCurrentDirectory
  , getCurrentDirectory
  , makeRelativeToCurrentDirectory
  , tryMakeRelativeToCurrent
  , canonicalizePath
  , createSymbolicLink

    -- * Existence Tests
  , doesFileExist
  , doesDirectoryExist

    -- * Timestamps
  , getModificationTime

    -- * File Operations
  , readUtf8
  , readBytes
  , writeUtf8
  , writeBytes
  , removeFile
  , copyFile
  , renameFile

    -- * Environment
  , findExecutable
  , lookupEnv
  , setEnv
  , unsetEnv

    -- * Pre-defined directories
  , getHomeDirectory

    -- * Concurrency
  , mapConcurrentlyE

    -- * Exceptions
  , ignoreIO

    -- * Temporary
  , withSystemTempDirectory
  ) where

import qualified Control.Concurrent.Async as Async
import           Control.Exception (IOException)
import           Control.Monad.Catch (MonadMask(..), MonadCatch(..), handle, throwM)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Trans.Maybe (MaybeT(..))

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time (UTCTime)

import           Mafia.Path

import           P

import qualified System.Directory as Directory
import qualified System.Environment as Environment
import           System.IO (IO)
import           System.IO.Error (isDoesNotExistError)
import qualified System.IO.Temp as Temp
import qualified System.Posix.Files as Posix

import           X.Control.Monad.Trans.Either (EitherT, pattern EitherT)
import           X.Control.Monad.Trans.Either (runEitherT, hoistEither)

------------------------------------------------------------------------
-- Directory Operations

data ListingOptions = Recursive | RecursiveDepth Int
  deriving (Eq, Ord, Show)

getDirectoryListing :: MonadIO m => ListingOptions -> Directory -> m [Path]
getDirectoryListing (RecursiveDepth n) _ | n < 0 = return []
getDirectoryListing options path = do
    entries    <- fmap (path </>) `liftM` getDirectoryContents path
    subEntries <- mapM down entries
    return (concat (entries : subEntries))
  where
    down entry = do
      isDir <- doesDirectoryExist entry
      if isDir
         then getDirectoryListing options' entry
         else return []

    options' = case options of
      Recursive        -> Recursive
      RecursiveDepth n -> RecursiveDepth (n-1)

getDirectoryContents :: MonadIO m => Directory -> m [Path]
getDirectoryContents path = liftIO $ do
  entries <- Directory.getDirectoryContents (T.unpack path)
  let interesting x = not (x == "." || x == "..")
  return . filter interesting
         . fmap T.pack
         $ entries

createDirectoryIfMissing :: MonadIO m => Bool -> Directory -> m ()
createDirectoryIfMissing parents dir =
  liftIO (Directory.createDirectoryIfMissing parents (T.unpack dir))

removeDirectoryRecursive :: MonadIO m => Directory -> m ()
removeDirectoryRecursive dir =
  liftIO (Directory.removeDirectoryRecursive (T.unpack dir))

renameDirectory :: MonadIO m => Directory -> Directory -> m ()
renameDirectory from to =
  liftIO (Directory.renameDirectory (T.unpack from) (T.unpack to))

setCurrentDirectory :: MonadIO m => Directory -> m ()
setCurrentDirectory dir = liftIO (Directory.setCurrentDirectory (T.unpack dir))

getCurrentDirectory :: MonadIO m => m Directory
getCurrentDirectory = T.pack `liftM` liftIO Directory.getCurrentDirectory

makeRelativeToCurrentDirectory :: MonadIO m => Path -> m (Maybe Path)
makeRelativeToCurrentDirectory path = do
  current <- getCurrentDirectory
  absPath <- T.pack `liftM` liftIO (Directory.makeAbsolute (T.unpack path))
  return (makeRelative current absPath)

tryMakeRelativeToCurrent :: MonadIO m => Directory -> m Directory
tryMakeRelativeToCurrent dir =
  fromMaybe dir `liftM` makeRelativeToCurrentDirectory dir

canonicalizePath :: MonadIO m => Path -> m Path
canonicalizePath path =
  T.pack `liftM` liftIO (Directory.canonicalizePath (T.unpack path))

createSymbolicLink :: MonadIO m => Path -> File -> m ()
createSymbolicLink src dst =
  liftIO $ Posix.createSymbolicLink (T.unpack src) (T.unpack dst)

------------------------------------------------------------------------
-- Existence Tests

doesFileExist :: MonadIO m => File -> m Bool
doesFileExist path = liftIO (Directory.doesFileExist (T.unpack path))

doesDirectoryExist :: MonadIO m => Directory -> m Bool
doesDirectoryExist path = liftIO (Directory.doesDirectoryExist (T.unpack path))

------------------------------------------------------------------------
-- Timestamps

getModificationTime :: MonadIO m => File -> m (Maybe UTCTime)
getModificationTime path =
  let
    onError e =
      if isDoesNotExistError e then
        return Nothing
      else
        throwM e
  in
    liftIO . handle onError . liftM Just $
      Directory.getModificationTime (T.unpack path)

------------------------------------------------------------------------
-- File I/O

readUtf8 :: MonadIO m => File -> m (Maybe Text)
readUtf8 path = runMaybeT $ do
  bytes <- MaybeT (readBytes path)
  return (T.decodeUtf8 bytes)

writeUtf8 :: MonadIO m => File -> Text -> m ()
writeUtf8 path text = liftIO (B.writeFile (T.unpack path) (T.encodeUtf8 text))

readBytes :: MonadIO m => File -> m (Maybe ByteString)
readBytes path = liftIO $ do
  exists <- doesFileExist path
  case exists of
    False -> return Nothing
    True  -> Just `liftM` B.readFile (T.unpack path)

writeBytes :: MonadIO m => File -> ByteString -> m ()
writeBytes path bytes = liftIO (B.writeFile (T.unpack path) bytes)

removeFile :: MonadIO m => File -> m ()
removeFile path = liftIO (Directory.removeFile (T.unpack path))

copyFile :: MonadIO m => File -> File -> m ()
copyFile src dst = liftIO (Directory.copyFile (T.unpack src) (T.unpack dst))

renameFile :: MonadIO m => File -> File -> m ()
renameFile src dst = liftIO (Directory.renameFile (T.unpack src) (T.unpack dst))

------------------------------------------------------------------------
-- Environment

findExecutable :: MonadIO m => Text -> m (Maybe File)
findExecutable name = liftIO $ do
  path <- Directory.findExecutable (T.unpack name)
  return (fmap T.pack path)

lookupEnv :: MonadIO m => Text -> m (Maybe Text)
lookupEnv key = liftIO $ do
  value <- Environment.lookupEnv (T.unpack key)
  return (fmap T.pack value)

setEnv :: MonadIO m => Text -> Text -> m ()
setEnv key value = liftIO $
  Environment.setEnv (T.unpack key) (T.unpack value)

unsetEnv :: MonadIO m => Text -> m ()
unsetEnv key = liftIO $
  Environment.unsetEnv (T.unpack key)

------------------------------------------------------------------------
-- Pre-defined directories

getHomeDirectory :: MonadIO m => m Directory
getHomeDirectory = T.pack `liftM` liftIO Directory.getHomeDirectory

------------------------------------------------------------------------
-- Concurrency

mapConcurrentlyE :: Traversable t => (a -> EitherT x IO b) -> t a -> EitherT x IO (t b)
mapConcurrentlyE io xs = do
  ys <- liftIO (Async.mapConcurrently (runEitherT . io) xs)
  hoistEither (sequence ys)

------------------------------------------------------------------------
-- Exceptions

ignoreIO :: MonadCatch m => m () -> m ()
ignoreIO =
  handle (\(_ :: IOException) -> return ())

------------------------------------------------------------------------
-- Temporary

withSystemTempDirectory :: (MonadIO m, MonadMask m) => Text -> (Directory -> EitherT x m a) -> EitherT x m a
withSystemTempDirectory template io =
  EitherT . Temp.withSystemTempDirectory (T.unpack template) $ \dir -> runEitherT (io (T.pack dir))
