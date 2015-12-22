{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Mafia.Cabal.Index
  ( repairIndexFile
  , readPackageName
  ) where

import           Control.Exception (IOException)
import           Control.Monad.Catch (catch)
import           Control.Monad.IO.Class (MonadIO(..))

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import qualified Codec.Archive.Tar as Tar

import           Mafia.Cabal.Types
import           Mafia.IO
import           Mafia.Path

import           P

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT, hoistEither)

------------------------------------------------------------------------

readIndexFile :: MonadIO m => File -> EitherT CabalError m ByteString
readIndexFile file = do
  mbs <- readBytes file
  hoistEither $ maybe (Left (IndexFileNotFound file)) Right mbs

readEntries :: Monad m => ByteString -> EitherT CabalError m [Tar.Entry]
readEntries bs = do
  let tar = Tar.read (L.fromStrict bs)
  hoistEither $ Tar.foldEntries (\x -> fmap (x :)) (Right []) (Left . CorruptIndexFile) tar

isTreeRef :: Tar.Entry -> Bool
isTreeRef x = Tar.entryPath x == "local-build-tree-reference/"

fromEntry :: Tar.Entry -> Maybe Directory
fromEntry x =
  case Tar.entryContent x of
    Tar.OtherEntryType _ lbs _ | isTreeRef x -> Just (T.decodeUtf32LE (L.toStrict lbs))
    _                                        -> Nothing

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

readPackageName :: MonadIO m => File -> m (Maybe Text)
readPackageName cabalFile = do
  text <- fromMaybe T.empty `liftM` readUtf8 cabalFile

  let findName ("name:":name:_) = Just name
      findName _                = Nothing

  return . listToMaybe
         . mapMaybe (findName . T.words)
         $ T.lines text
