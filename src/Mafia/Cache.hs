{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Mafia.Cache (
    CacheError(..)
  , renderCacheError

  , pkgKey

  , CacheEnv(..)
  , getCacheEnv
  , userCacheDirectory
  , relativeCacheDirectory
  , cacheVersion

  , Flavour(..)
  , renderFlavour
  , renderFlavourSuffix

  , PackageKey(..)
  , renderPackageKey
  , parsePackageKey

  , withPackageLock
  , withPackageLock_

  , packageConfig
  , packageFlavourMarker
  , packageSandboxConfig
  , packageSourceDir
  , packageSandboxDir

  , ExportResult(..)
  , renderExportResult

  , ImportResult(..)
  , renderImportResult

  , exportPackage
  , importPackage
  , listPackages
  ) where

import           Control.Monad.IO.Class (MonadIO(..))

import qualified Data.Text as Text
import qualified Data.Text.IO as Text

import           Mafia.Cabal.Types
import           Mafia.Flock
import           Mafia.Ghc
import           Mafia.Hash
import           Mafia.Home
import           Mafia.IO
import           Mafia.Package
import           Mafia.Path
import           Mafia.Process

import           P

import           System.IO (IO, stderr)

import           X.Control.Monad.Trans.Either (EitherT, left, hoistEither)


data CacheEnv =
  CacheEnv {
      envGhcTarget :: !GhcTarget
    , envGhcVersion :: !GhcVersion
    , envMafiaHome :: !Directory
    } deriving (Eq, Ord, Show)

data PackageKey =
  PackageKey !PackageId !Hash
  deriving (Eq, Ord, Show)

data Flavour =
    Vanilla
  | Profiling
  | Documentation
    deriving (Eq, Ord, Show)

data ImportResult =
    ImportExists !PackageKey
  | ImportSuccess !PackageKey
  | ImportFailed !PackageKey
    deriving (Eq, Ord, Show)

data ExportResult =
    ExportExists !PackageKey
  | ExportSuccess !PackageKey
    deriving (Eq, Ord, Show)

data CacheError =
    CacheGhcError !GhcError
  | CacheProcessError !ProcessError
  | CacheCannotParsePackageId !Text !Text
  | CacheCannotParseHash !Text !Text
  | CacheExportDirectoryDoesNotExist !Directory
  | CacheImportPackageDoesNotExist !Path
    deriving (Show)

renderCacheError :: CacheError -> Text
renderCacheError = \case
  CacheGhcError err ->
    renderGhcError err
  CacheProcessError err ->
    renderProcessError err
  CacheCannotParsePackageId txt pid ->
    "Failed to parse the package-id <" <> pid <> "> in the package-key <" <> txt <> ">"
  CacheCannotParseHash txt hash ->
    "Failed to parse the hash <" <> hash <> "> in the package-key <" <> txt <> ">"
  CacheExportDirectoryDoesNotExist output ->
    "Cannot export package, the export directory does not exist: " <> output
  CacheImportPackageDoesNotExist input ->
    "Cannot import package, the tarball does not exist: " <> input

renderPackageKey :: PackageKey -> Text
renderPackageKey (PackageKey pid hash) =
  renderPackageId pid <> "-" <> renderHash hash

parsePackageKey :: Text -> Either CacheError PackageKey
parsePackageKey txt = do
  let
    (pid0, hash0) =
      first (Text.dropEnd 1) $
      Text.breakOnEnd "-" txt

  pid <- maybeToRight (CacheCannotParsePackageId txt pid0) $ parsePackageId pid0
  hash <- maybeToRight (CacheCannotParseHash txt hash0) $ parseHash hash0

  pure $ PackageKey pid hash

renderFlavour :: Flavour -> Text
renderFlavour = \case
  Vanilla ->
    "vanilla"
  Profiling ->
    "profiling"
  Documentation ->
    "documentation"

renderFlavourSuffix :: Flavour -> Text
renderFlavourSuffix = \case
  Vanilla ->
    ""
  Profiling ->
    " [profiling]"
  Documentation ->
    " [documentation]"

renderImportResult :: ImportResult -> Text
renderImportResult = \case
  ImportExists key ->
    "Skipped " <> renderPackageKey key
  ImportSuccess key ->
    "Imported " <> renderPackageKey key
  ImportFailed key ->
    "Failed to import " <> renderPackageKey key

renderExportResult :: ExportResult -> Text
renderExportResult = \case
  ExportExists key ->
    "Skipped " <> renderPackageKey key
  ExportSuccess key ->
    "Exported " <> renderPackageKey key

pkgKey :: Package -> PackageKey
pkgKey p =
  PackageKey (refId $ pkgRef p) (pkgHash p)

-- the package cache path includes a version number in case the contents or
-- layout of the cache changes in subsequent mafia versions.
cacheVersion :: Int
cacheVersion =
  2

userCacheDirectory :: CacheEnv -> Directory
userCacheDirectory env =
  envMafiaHome env </> relativeCacheDirectory env

relativeCacheDirectory :: CacheEnv -> Directory
relativeCacheDirectory env =
  "packages"
    </> Text.pack (show cacheVersion)
    </> unGhcTarget (envGhcTarget env)
    </> renderGhcVersion (envGhcVersion env)

getCacheEnv :: EitherT CacheError IO CacheEnv
getCacheEnv = do
  target <- firstT CacheGhcError getGhcTarget
  version <- firstT CacheGhcError getGhcVersion
  home <- getMafiaHome
  return (CacheEnv target version home)

withPackageLock :: CacheEnv -> PackageKey -> Flavour -> EitherT x IO a -> EitherT x IO a
withPackageLock env key f =
  withFileLock (packageLockPath env key) $ do
    liftIO . Text.hPutStrLn stderr $
      "Waiting for " <> renderPackageKey key <> renderFlavourSuffix f

withPackageLock_ :: CacheEnv -> PackageKey -> EitherT x IO a -> EitherT x IO a
withPackageLock_ env key =
  withPackageLock env key Vanilla

packageConfig :: CacheEnv -> PackageKey -> File
packageConfig env key =
  packageSandboxDir env key </> "package.conf"

packageFlavourMarker :: CacheEnv -> PackageKey -> Flavour -> File
packageFlavourMarker env key flav =
  packageSandboxDir env key </> "package." <> renderFlavour flav

packageLockPath :: CacheEnv -> PackageKey -> File
packageLockPath env key =
  userCacheDirectory env </> ".locks" </> renderPackageKey key

packageSandboxDir :: CacheEnv -> PackageKey -> SandboxDir
packageSandboxDir env key =
  userCacheDirectory env </> renderPackageKey key

packageSandboxConfig :: CacheEnv -> PackageKey -> SandboxConfigFile
packageSandboxConfig env key =
  packageSandboxDir env key </> "sandbox.config"

packageSourceDir :: CacheEnv -> PackageKey -> Directory
packageSourceDir env key =
  packageSandboxDir env key </> "src"

exportPackage :: CacheEnv -> PackageKey -> Directory -> EitherT CacheError IO ExportResult
exportPackage env key output0 = do
  unlessM (doesDirectoryExist output0) $
    left $ CacheExportDirectoryDoesNotExist output0

  let
    outputDir =
      output0 </> relativeCacheDirectory env

    output =
      outputDir </> renderPackageKey key <> ".tar.gz"

  done <- doesFileExist output

  if done then
    pure $ ExportExists key
  else do
    createDirectoryIfMissing True outputDir

    call_ CacheProcessError "tar" [
        "cfz"
      , output
      , "-C"
      , userCacheDirectory env
      , renderPackageKey key
      ]

    pure $ ExportSuccess key

importPackage :: CacheEnv -> PackageKey -> Directory -> EitherT CacheError IO ImportResult
importPackage env key input0 = do
  let
    input =
      input0 </> relativeCacheDirectory env </> renderPackageKey key <> ".tar.gz"

  unlessM (doesFileExist input) $
    left $ CacheImportPackageDoesNotExist input

  withPackageLock_ env key $ do
    let
      mark =
        packageFlavourMarker env key Vanilla

      sbdir =
        packageSandboxDir env key

    done <- doesFileExist mark

    if done then
      pure $ ImportExists key
    else do
      ignoreIO $ removeDirectoryRecursive sbdir
      call_ CacheProcessError "tar" [
          "xf"
        , input
        , "-C"
        , userCacheDirectory env
        ]
      success <- doesDirectoryExist sbdir
      if success then
        pure $ ImportSuccess key
      else
        pure $ ImportFailed key

listPackages :: CacheEnv -> Directory -> EitherT CacheError IO [PackageKey]
listPackages env input0 = do
  let
    input =
      input0 </> relativeCacheDirectory env

  ok <- doesDirectoryExist input

  if ok then do
    xs <- getDirectoryContents input
    hoistEither $ traverse (parsePackageKey . takeBaseName . takeBaseName) xs
  else
    pure []
