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

import           System.IO (IO)
import           System.IO.Temp (withTempDirectory)

import           X.Control.Monad.Trans.Either (EitherT, pattern EitherT, runEitherT)


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
  let path = bin </> nv
  -- This is to support old versions of mafia that had installed a single executable
  -- We can remove this at some point
  -- NOTE: This makes no attempt at being threadsafe
  liftIO (doesFileExist path) >>= \b -> when b $ do
    renameFile path (path <> ".tmp")
    createDirectoryIfMissing True path
    renameFile (path <> ".tmp") (path </> (unPackageName . pkgName) package)
  liftIO (doesDirectoryExist path) >>= \case
    True ->
      pure path
    False -> do
      EitherT . withTempDirectory (T.unpack tmp) (T.unpack $ nv <> ".") $ \sandboxDir -> runEitherT $ do
        Pass <- callFrom id (T.pack sandboxDir) "cabal" ["sandbox", "init"]
        -- Install any required executables first
        -- This could also recursively call `installBinary` and copy the output in to the sandbox if we do this again
        for_ deps $ \p -> do
          Pass <- callFrom id (T.pack sandboxDir) "cabal" ["install", renderPackageId p]
          pure ()
        Pass <- callFrom id (T.pack sandboxDir) "cabal" ["install", nv]
        createDirectoryIfMissing True path
        getDirectoryListing (RecursiveDepth 1) (T.pack sandboxDir </> ".cabal-sandbox" </> "bin") >>= \p ->
          for_ p $ \f -> copyFile f (path </> takeFileName f)
        return path

ensureExeOnPath :: PackageId -> EitherT ProcessError IO ()
ensureExeOnPath pkg = do
  dir <- installBinary pkg []
  setEnv "PATH" . maybe dir (\path -> dir <> ":" <> path) =<< lookupEnv "PATH"
