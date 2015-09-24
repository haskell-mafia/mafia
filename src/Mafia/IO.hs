{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Mafia.IO
  ( -- * Directory Operations
    getDirectoryContents

    -- * Existence Tests
  , doesFileExist
  , doesDirectoryExist

    -- * File Operations
  , readText
  , readBytes
  , writeText
  , writeBytes
  ) where

import           Control.Monad.IO.Class (MonadIO(..))

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Mafia.Path

import           P

import qualified System.Directory as Directory

------------------------------------------------------------------------
-- Filesystem Operations

getDirectoryContents :: MonadIO m => Directory -> m [File]
getDirectoryContents path = liftIO $ do
  entries <- Directory.getDirectoryContents (T.unpack path)
  let interesting x = not (x == "." || x == "..")
  return . filter interesting
         . fmap T.pack
         $ entries

doesFileExist :: MonadIO m => File -> m Bool
doesFileExist path = liftIO (Directory.doesFileExist (T.unpack path))

doesDirectoryExist :: MonadIO m => Directory -> m Bool
doesDirectoryExist path = liftIO (Directory.doesDirectoryExist (T.unpack path))

------------------------------------------------------------------------
-- File I/O

readText :: MonadIO m => File -> m Text
readText path = liftIO (T.readFile (T.unpack path))

writeText :: MonadIO m => File -> Text -> m ()
writeText path text = liftIO (T.writeFile (T.unpack path) text)

readBytes :: MonadIO m => File -> m ByteString
readBytes path = liftIO (B.readFile (T.unpack path))

writeBytes :: MonadIO m => File -> ByteString -> m ()
writeBytes path bytes = liftIO (B.writeFile (T.unpack path) bytes)
