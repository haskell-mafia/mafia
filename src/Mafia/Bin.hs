{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module Mafia.Bin
  ( installBinary
  , ensureExeOnPath
  ) where

import           Control.Monad.IO.Class (MonadIO(..))

import qualified Data.Text as T

import           Mafia.Home
import           Mafia.IO
import           Mafia.Install
import           Mafia.Package
import           Mafia.Path
import           P

import           System.IO (IO)
import           System.Posix.Files (createSymbolicLink)

import           X.Control.Monad.Trans.Either (EitherT)


-- | Installs a given cabal package at a specific version and return a directory containing all executables
installBinary :: PackageId -> EitherT InstallError IO Directory
installBinary pid = do
  bin <- ensureMafiaDir "bin"

  let
    plink = bin </> renderPackageId pid
    pdir = plink <> "/"
    pbin = plink <> "/bin"

  unlessM (doesDirectoryExist pdir) $ do
    -- if the directory doesn't exist, but there happens to be a file there, we
    -- must have a dead symlink, so lets remove it and install it again.
    ignoreIO $ removeFile plink

    pkg <- installPackage pid
    env <- getPackageEnv
    let gdir = packageSandboxDir env pkg
    liftIO $ createSymbolicLink (T.unpack gdir) (T.unpack plink)

  return pbin

ensureExeOnPath :: PackageId -> EitherT InstallError IO ()
ensureExeOnPath pkg = do
  dir <- installBinary pkg
  setEnv "PATH" . maybe dir (\path -> dir <> ":" <> path) =<< lookupEnv "PATH"
