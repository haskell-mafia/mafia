{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Mafia.Cabal.Index
  ( repairIndexFile
  , createIndexFile
  ) where

import           Control.Exception (IOException)
import           Control.Monad.Catch (catch)
import           Control.Monad.IO.Class (MonadIO(..))

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import qualified Data.Text.Encoding as T

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Entry as Tar

import           Mafia.Cabal.Types
import           Mafia.IO
import           Mafia.Path

import           P

import qualified Prelude as Prelude

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT, hoistEither)

------------------------------------------------------------------------

readIndexFile :: MonadIO m => File -> EitherT CabalError m ByteString
readIndexFile file = do
  mbs <- readBytes file
  hoistEither $ maybe (Left (CabalIndexFileNotFound file)) Right mbs

readEntries :: Monad m => ByteString -> EitherT CabalError m [Tar.Entry]
readEntries bs = do
  let tar = Tar.read (L.fromStrict bs)
  hoistEither $ Tar.foldEntries (\x -> fmap (x :)) (Right []) (Left . CabalCorruptIndexFile) tar

isTreeRef :: Tar.Entry -> Bool
isTreeRef x = Tar.entryPath x == "local-build-tree-reference/"

treeRef :: Tar.TarPath
treeRef =
  -- This conversion cannot fail because the path is shorter than 255 characters.
  either (Prelude.error "Mafia.Cabal.Index.treeRef: impossible") id
         (Tar.toTarPath True "local-build-tree-reference")

fromEntry :: Tar.Entry -> Maybe Directory
fromEntry x =
  case Tar.entryContent x of
    Tar.OtherEntryType _ lbs _ | isTreeRef x -> Just (T.decodeUtf32LE (L.toStrict lbs))
    _                                        -> Nothing

fromDirectory :: Directory -> Tar.Entry
fromDirectory dir =
  let lbs     = L.fromStrict (T.encodeUtf32LE dir)
      content = Tar.OtherEntryType 'C' lbs (L.length lbs)
  in Tar.simpleEntry treeRef content

------------------------------------------------------------------------

repairIndexFile :: SandboxDir -> EitherT CabalError IO ()
repairIndexFile = filterEntries doesDirectoryExist

filterEntries :: (Directory -> IO Bool) -> File -> EitherT CabalError IO ()
filterEntries pred sandboxDir = do
  let indexFile  = sandboxDir </> "packages/00-index.tar"
      indexCache = sandboxDir </> "packages/00-index.cache"

  bytes   <- readIndexFile indexFile
  entries <- readEntries bytes

  let pred' = maybe (pure True) pred . fromEntry
  entries' <- liftIO (filterM pred' entries)

  when (length entries /= length entries') $ do
    let bytes' = L.toStrict (Tar.write entries')
    writeBytes indexFile bytes'
    removeFile indexCache `catch` \(_ :: IOException) -> pure ()

------------------------------------------------------------------------

-- This will replace/overwrite and existing index file
createIndexFile :: [Directory] -> SandboxDir -> EitherT CabalError IO ()
createIndexFile sources sandboxDir = do
  let packagesDir = sandboxDir  </> "packages"
      indexFile   = packagesDir </> "00-index.tar"

  removeDirectoryRecursive packagesDir
  createDirectoryIfMissing True packagesDir

  let entries = fmap fromDirectory sources
      bytes   = L.toStrict (Tar.write entries)

  writeBytes indexFile bytes
