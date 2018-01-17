{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module Mafia.Bin
  ( BinError(..)
  , renderBinError

  , InstallPackage(..)
  , ipackageId
  , renderInstallPackage
  , ipkgName
  , ipkgConstraints

  , installBinary
  , installOnPath
  ) where

import           Control.Monad.Trans.Either (EitherT, left)
import           Control.Monad.Trans.Bifunctor (firstT)

import           Mafia.Cabal.Constraint
import           Mafia.Cabal.Sandbox
import           Mafia.Cabal.Types
import           Mafia.Cache
import           Mafia.Hash
import           Mafia.Home
import           Mafia.Install
import           Mafia.IO
import           Mafia.Package
import           Mafia.Path
import           Mafia.P

import           System.IO (IO)

data BinError =
    BinInstallError InstallError
  | BinCacheError CacheError
  | BinCabalError CabalError
  | BinNotExecutable PackageId
  | BinFailedToCreateSymbolicLink Path File
    deriving (Show)

renderBinError :: BinError -> Text
renderBinError = \case
  BinInstallError err ->
    renderInstallError err
  BinCacheError err ->
    renderCacheError err
  BinCabalError err ->
    renderCabalError err
  BinNotExecutable pid ->
    "Cannot link bin/ directory for " <> renderPackageId pid <> " as no executables were installed."
  BinFailedToCreateSymbolicLink src dst ->
    "Failed to create symbolic link from " <> src <> " -> " <> dst <> "."

data InstallPackage =
    InstallPackageName PackageName
  | InstallPackageId PackageId
    deriving (Eq, Ord, Show)

ipackageId :: Text -> [Int] -> InstallPackage
ipackageId name ver =
  InstallPackageId (packageId name ver)

renderInstallPackage :: InstallPackage -> Text
renderInstallPackage = \case
  InstallPackageName name ->
    unPackageName name
  InstallPackageId pid ->
    renderPackageId pid

ipkgName :: InstallPackage -> PackageName
ipkgName = \case
  InstallPackageName name ->
    name
  InstallPackageId pid ->
    pkgName pid

ipkgConstraints :: InstallPackage -> [Constraint]
ipkgConstraints = \case
  InstallPackageName _ ->
    []
  InstallPackageId (PackageId name ver) ->
    [ConstraintPackage name ver]

destinationOfInstallPackage :: InstallPackage -> [Constraint] -> Text
destinationOfInstallPackage ipkg = \case
  [] ->
    renderInstallPackage ipkg
  cs ->
    renderInstallPackage ipkg <> "-" <> renderHash (hashConstraints cs)

hashConstraints :: [Constraint] -> Hash
hashConstraints =
  hashHashes . fmap (hashText . renderConstraint)

-- | Installs a given cabal package at a specific version and return a directory containing all executables
installBinary :: InstallPackage -> [Constraint] -> EitherT BinError IO Directory
installBinary ipkg constraints = do
  bin <- ensureMafiaDir "bin"
  installInDirectory bin ipkg constraints

installOnPath :: InstallPackage -> [Constraint] -> EitherT BinError IO ()
installOnPath pkg constraints = do
  sandboxBin <- (</> "bin") <$> firstT BinCabalError initSandbox
  createDirectoryIfMissing False sandboxBin
  pkgBin <- installInDirectory sandboxBin pkg constraints
  prependPath pkgBin

installInDirectory :: Directory -> InstallPackage -> [Constraint] -> EitherT BinError IO Directory
installInDirectory bin ipkg constraints = do
  let
    plink = bin </> destinationOfInstallPackage ipkg constraints
    pdir = plink <> "/"
    pbin = plink <> "/bin"

  unlessM (doesDirectoryExist pdir) $ do
    -- if the directory doesn't exist, but there happens to be a file there, we
    -- must have a dead symlink, so lets remove it and install it again.
    ignoreIO $ removeFile plink

    pkg <-
      firstT BinInstallError $
      installPackage (ipkgName ipkg) (ipkgConstraints ipkg <> constraints)

    gdir <-
      fmap (flip packageSandboxDir $ pkgKey pkg) .
      firstT BinCacheError $
      getCacheEnv

    unlessM (doesDirectoryExist $ gdir </> "bin") $
      left (BinNotExecutable . refId $ pkgRef pkg)

    -- There can easily be a race where two processes are trying to install the
    -- same binary, so catch any errors that occur when trying to create the
    -- symbolic link.
    ignoreIO $ createSymbolicLink gdir plink

    -- We can't check that 'plink' points to 'gdir' as the other process may
    -- have built the same package, but with a different hash, and this is ok.
    -- Instead, we'll need to settle for checking that the bin/ directory
    -- exists.
    unlessM (doesDirectoryExist $ plink </> "bin") $
      left (BinFailedToCreateSymbolicLink gdir plink)

  return pbin
