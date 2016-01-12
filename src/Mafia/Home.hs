{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module Mafia.Home
  ( getMafiaHome
  , getMafiaDir
  , ensureMafiaDir
  , installBinary
  , ensureExeOnPath
  ) where

import           Control.Monad.IO.Class (MonadIO(..))

import qualified Data.Text as T

import           Mafia.IO
import           Mafia.Package
import           Mafia.Path
import           Mafia.Process

import           P

import           System.FileLock (SharedExclusive (..), lockFile, unlockFile)
import           System.IO (IO)
import           System.IO.Temp (withTempDirectory)

import           X.Control.Monad.Trans.Either (EitherT, pattern EitherT, runEitherT, bracketEitherT')


getMafiaHome :: MonadIO m => m Directory
getMafiaHome = do
  mhome <- lookupEnv "MAFIA_HOME"
  case mhome of
    Just home -> return home
    Nothing   -> (</> T.pack ".ambiata/mafia") `liftM` getHomeDirectory

getMafiaDir :: MonadIO m => Directory -> m Directory
getMafiaDir path = do
  home <- getMafiaHome
  return (home </> path)

ensureMafiaDir :: MonadIO m => Directory -> m Directory
ensureMafiaDir path = do
  path' <- getMafiaDir path
  createDirectoryIfMissing True path'
  return path'

-- | Installs a given cabal package at a specific version and return a directory containing all executables
installBinary :: PackageId -> [PackageId] -> EitherT ProcessError IO Directory
installBinary package deps = do
  tmp <- ensureMafiaDir "tmp"
  let nv = renderPackageId package
  bin <- ensureMafiaDir "bin"
  let prefix = bin </> nv
      path = prefix </> "bin"
      lock = T.unpack (prefix </> "lock")
      marker = prefix </> "installed"
      withLock = bracketEitherT' (liftIO (lockFile lock Exclusive)) (liftIO . unlockFile) . const
      installArgs p = [ "install", renderPackageId p, "--prefix=" <> prefix ]
  -- This is to support old versions of mafia that had installed a single executable
  -- We can remove this at some point
  -- NOTE: This makes no attempt at being threadsafe
  whenM (liftIO (doesFileExist prefix)) (removeFile prefix)
  createDirectoryIfMissing True prefix
  unlessM (liftIO (doesFileExist marker)) . withLock $
    unlessM (liftIO (doesFileExist marker)) $
      EitherT . withTempDirectory (T.unpack tmp) (T.unpack $ nv <> ".") $ \sandboxDir -> runEitherT $ do
        -- Build the binary and its dependencies in a new sandbox.
        -- Sandbox necessary as binary's deps could clash with the local project
        Pass <- callFrom id (T.pack sandboxDir) "cabal" ["sandbox", "init"]
        -- Install any required executables first
        for_ deps ensureExeOnPath
        Pass <- callFrom id (T.pack sandboxDir) "cabal" (installArgs package)
        writeBytes marker mempty
        pure ()
  pure path

ensureExeOnPath :: PackageId -> EitherT ProcessError IO ()
ensureExeOnPath pkg = do
  dir <- installBinary pkg []
  setEnv "PATH" . maybe dir (\path -> dir <> ":" <> path) =<< lookupEnv "PATH"
