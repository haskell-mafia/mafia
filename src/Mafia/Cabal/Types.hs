{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Mafia.Cabal.Types
  ( SandboxDir
  , SandboxConfigFile

  , Flag(..)
  , renderFlag
  , parseFlag

  , Package(..)
  , PackageRef(..)
  , SourcePackage(..)
  , mkPackage
  , renderPackageRef
  , renderHashId

  , PackagePlan(..)
  , PackageStatus(..)
  , PackageChange(..)
  , renderPackagePlan
  , renderPackageStatus
  , renderPackageChange

  , CabalError(..)
  , renderCabalError
  ) where

import qualified Codec.Archive.Tar as Tar

import           Data.Aeson (Value(..), ToJSON(..), FromJSON(..), (.:), (.=), object)
import           Data.Char (isAlpha)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T

import           Mafia.Ghc
import           Mafia.Hash
import           Mafia.Path
import           Mafia.Process
import           Mafia.Project
import           Mafia.Package

import           P

------------------------------------------------------------------------

type SandboxDir = Directory

type SandboxConfigFile = File

data Flag =
    FlagOff !Text
  | FlagOn !Text
    deriving (Eq, Ord, Show)

data SourcePackage =
  SourcePackage {
      spDirectory :: !Directory
    , spPackageId :: !PackageId
    , spHash      :: !Hash
    } deriving (Eq, Ord, Show)

data PackageRef =
  PackageRef {
      refId     :: !PackageId
    , refFlags  :: ![Flag]
    , refSrcPkg :: !(Maybe SourcePackage)
    } deriving (Eq, Ord, Show)

data Package =
  Package {
      pkgRef  :: !PackageRef
    , pkgDeps :: !(Set Package)
    , pkgHash :: !Hash
    } deriving (Show)

instance Eq Package where
  (==) (Package ref0 _ hash0) (Package ref1 _ hash1) =
    ref0 == ref1 &&
    hash0 == hash1

instance Ord Package where
  compare (Package ref0 _ hash0) (Package ref1 _ hash1) =
    compare
      (ref0, hash0)
      (ref1, hash1)

data PackageChange =
  PackageChange {
      pcPackageId  :: !PackageId
    , pcNewVersion :: !Version
    } deriving (Eq, Ord, Show)

data PackageStatus =
    NewPackage
  | NewVersion
  | Reinstall ![PackageChange]
    deriving (Eq, Ord, Show)

data PackagePlan =
  PackagePlan {
      ppRef    :: !PackageRef
    , ppLatest :: !(Maybe Version)
    , ppDeps   :: ![PackageId]
    , ppStatus :: !PackageStatus
    } deriving (Eq, Ord, Show)

------------------------------------------------------------------------

mkPackage :: PackageRef -> Set Package -> Package
mkPackage ref deps =
  Package ref deps (hashPackage ref deps)

renderPackageRef :: PackageRef -> Text
renderPackageRef = \case
  PackageRef pid flags Nothing ->
    renderPackageId pid <> renderFlagsSuffix flags
  PackageRef pid flags (Just (SourcePackage _ _ hash)) ->
    renderPackageId pid <> "-" <> renderHash hash <> renderFlagsSuffix flags

renderFlagsSuffix :: [Flag] -> Text
renderFlagsSuffix = \case
  [] ->
    T.empty
  xs ->
    " " <> T.intercalate " " (fmap renderFlag xs)

renderHashId :: Package -> Text
renderHashId (Package (PackageRef pid _ _) _ hash) =
  renderPackageId pid <> "-" <> renderHash hash

hashPackage :: PackageRef -> Set Package -> Hash
hashPackage ref deps =
  hashHashes $ hashPackageRef ref : fmap pkgHash (Set.toList deps)

hashPackageRef :: PackageRef -> Hash
hashPackageRef (PackageRef pid flags msrc) =
  case msrc of
    Nothing  -> hashHashes [hashPackageId pid, hashFlags flags]
    Just src -> hashHashes [hashPackageId pid, hashFlags flags, spHash src]

hashPackageId :: PackageId -> Hash
hashPackageId =
  hashText . renderPackageId

hashFlags :: [Flag] -> Hash
hashFlags =
  hashText . T.unwords . fmap renderFlag

renderFlag :: Flag -> Text
renderFlag = \case
  FlagOff f -> "-" <> f
  FlagOn  f -> "+" <> f

parseFlag :: Alternative f => Text -> f Flag
parseFlag f0 =
  case T.uncons f0 of
    Just ('-', f1) ->
      pure $ FlagOff f1
    Just ('+', f1) ->
      pure $ FlagOn f1
    Just (x, _) | isAlpha x ->
      pure $ FlagOn f0
    Just _ ->
      empty
    Nothing ->
      empty

renderPackagePlan :: PackagePlan -> Text
renderPackagePlan (PackagePlan (PackageRef pid fs _) latest deps status) =
  mconcat
   [ renderPackageId pid
   , case latest of
       Nothing  -> ""
       Just ver -> " (latest: " <> renderVersion ver <> ")"
   , mconcat $ fmap (\f -> " " <> renderFlag f) fs
   , case deps of
       [] -> ""
       _  -> " (via: " <> T.intercalate " " (fmap renderPackageId deps) <> ")"
   , " " <> renderPackageStatus status
   ]

renderPackageStatus :: PackageStatus -> Text
renderPackageStatus = \case
  NewPackage ->
    "(new package)"
  NewVersion ->
    "(new version)"
  Reinstall cs ->
    "(reinstall) (changes: " <> T.intercalate ", " (fmap renderPackageChange cs) <> ")"

renderPackageChange :: PackageChange -> Text
renderPackageChange = \case
  PackageChange pid ver ->
    renderPackageId pid <> " -> " <> renderVersion ver

------------------------------------------------------------------------

type MinVersion = Version
type MaxVersion = Version

data CabalError =
    CabalProcessError ProcessError
  | CabalProjectError ProjectError
  | CabalGhcError GhcError
  | CabalHashError HashError
  | CabalParseError Text
  | CabalIndexFileNotFound File
  | CabalCorruptIndexFile Tar.FormatError
  | CabalSandboxConfigFileNotFound SandboxConfigFile
  | CabalSandboxConfigFieldNotFound SandboxConfigFile Text
  | CabalInstallIsNotReferentiallyTransparent
  | CabalSDistFailed Directory
  | CabalSDistFailedCouldNotReadFile Directory File
  | CabalReinstallsDetected [PackagePlan]
  | CabalFileNotFound Directory
  | CabalCouldNotReadPackageId File
  | CabalCouldNotReadPackageType File
  | CabalCouldNotParseVersion Text
  | CabalNoTopLevelPackage
  | CabalTopLevelPackageNotFoundInPlan PackageId
  | CabalMultipleTopLevelPackages [PackageId]
  | CabalInvalidVersion Version MinVersion MaxVersion
  | CabalNotInstalled
    deriving (Show)

renderCabalError :: CabalError -> Text
renderCabalError = \case
  CabalProcessError e ->
    renderProcessError e

  CabalProjectError e ->
    renderProjectError e

  CabalGhcError e ->
    renderGhcError e

  CabalHashError e ->
    renderHashError e

  CabalParseError err ->
    "Parse error: " <> err

  CabalIndexFileNotFound file ->
    "Index file not found: " <> file

  CabalCorruptIndexFile tarError ->
    "Corrupt index file: " <> T.pack (show tarError)

  CabalSandboxConfigFileNotFound file ->
    "Sandbox config file not found: " <> file

  CabalSandboxConfigFieldNotFound file field ->
    "Sandbox config field not found: " <> field <> " (in " <> file <> ")"

  CabalInstallIsNotReferentiallyTransparent ->
    "The impossible happened, cabal-install gave different answers on subsequent dry runs"

  CabalSDistFailed dir ->
    "Failed to run 'cabal sdist' for source package: " <> dir

  CabalSDistFailedCouldNotReadFile dir path ->
    "Failed to run 'cabal sdist' for source package: " <> dir <> "\n" <>
    "Could not read file: " <> path

  CabalFileNotFound dir ->
    "Could not find .cabal file in: " <> dir

  CabalCouldNotReadPackageId cabalFile ->
    "Failed to read package-id from: " <> cabalFile

  CabalCouldNotReadPackageType cabalFile ->
    "Failed to find 'library' or 'executable' stanzas in: " <> cabalFile

  CabalCouldNotParseVersion out ->
    "Could not parse or read the cabal-install version number from the following output:\n" <> out

  CabalNoTopLevelPackage ->
    "No top level package found after parsing install plan"

  CabalTopLevelPackageNotFoundInPlan pid ->
    "The top level package (" <> renderPackageId pid <> ") was not found in the install plan"

  CabalMultipleTopLevelPackages refs ->
    "Unexpectedly found multiple top level packages after parsing install plan: " <>
    T.intercalate ", " (fmap renderPackageId refs)

  CabalReinstallsDetected pps ->
    "Cabal's install plan suggested the following reinstalls:" <>
    mconcat (fmap (\pp -> "\n  " <> renderPackagePlan pp) pps) <>
    "\nReinstalls are not allowed as it would break the global package cache."

  CabalInvalidVersion ver vmin vmax ->
    mconcat
      [ "cabal-install ", renderVersion ver, " is not supported "
      , "(must be >= ", renderVersion vmin, " && < ", renderVersion vmax, ")" ]

  CabalNotInstalled ->
    mconcat
      [ "cabal-install is not installed."
      , "\nYou can download it from https://www.haskell.org/cabal/download.html" ]

------------------------------------------------------------------------

instance ToJSON SourcePackage where
  toJSON (SourcePackage d p h) =
    object
      [ "directory"  .= d
      , "package-id" .= p
      , "hash"       .= h ]

instance FromJSON SourcePackage where
  parseJSON = \case
    Object o ->
      SourcePackage <$>
        o .: "directory" <*>
        o .: "package-id" <*>
        o .: "hash"
    _ ->
      mzero
