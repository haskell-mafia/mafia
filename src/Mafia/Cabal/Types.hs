{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Mafia.Cabal.Types
  ( SandboxDir
  , SandboxConfigFile

  , Package(..)
  , PackageRef(..)
  , SourcePackage(..)
  , mkPackage
  , renderHashId

  , Flag(..)
  , renderFlag

  , CabalError(..)
  , renderCabalError
  ) where

import qualified Codec.Archive.Tar as Tar

import           Data.Aeson (Value(..), ToJSON(..), FromJSON(..), (.:), (.=), object)
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
    FlagOff Text
  | FlagOn  Text
    deriving (Eq, Ord, Show)

data SourcePackage =
  SourcePackage {
      spDirectory :: Directory
    , spPackageId :: PackageId
    , spHash      :: Hash
    } deriving (Eq, Ord, Show)

data PackageRef =
  PackageRef {
      refId     :: PackageId
    , refFlags  :: [Flag]
    , refSrcPkg :: Maybe SourcePackage
    } deriving (Eq, Ord, Show)

data Package =
  Package {
      pkgRef  :: PackageRef
    , pkgDeps :: [Package]
    , pkgHash :: Hash
    } deriving (Eq, Ord, Show)

------------------------------------------------------------------------

mkPackage :: PackageRef -> [Package] -> Package
mkPackage ref deps =
  Package ref deps (hashPackage ref deps)

renderHashId :: Package -> Text
renderHashId (Package (PackageRef pid _ _) _ hash) =
  renderPackageId pid <> "-" <> renderHash hash

hashPackage :: PackageRef -> [Package] -> Hash
hashPackage ref deps =
  hashHashes (hashPackageRef ref : fmap pkgHash deps)

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
  | CabalCouldNotParseVersion Text
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

  CabalCouldNotParseVersion out ->
    "Could not parse or read the cabal-install version number from the following output:\n" <> out

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
