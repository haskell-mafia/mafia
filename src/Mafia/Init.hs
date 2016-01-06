{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Mafia.Init
  ( Profiling(..)
  , initialize

  , InitError(..)
  , renderInitError
  ) where

import           Control.Monad.IO.Class (MonadIO(..))

import qualified Data.Aeson as A
import           Data.Aeson (Value(..), ToJSON(..), FromJSON(..), (.:), (.=))
import qualified Data.ByteString.Lazy as L
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Mafia.Cabal
import           Mafia.Git
import           Mafia.Hash
import           Mafia.IO
import           Mafia.Install
import           Mafia.Path
import           Mafia.Process
import           Mafia.Submodule

import           P

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT, left, hoistEither, firstEitherT)


data InitError =
    InitHashError HashError
  | InitGitError GitError
  | InitCabalError CabalError
  | InitSubmoduleError SubmoduleError
  | InitInstallError InstallError
  | InitParseError File Text
  | InitCabalFileNotFound Directory
    deriving (Show)

renderInitError :: InitError -> Text
renderInitError = \case
  InitHashError e ->
    renderHashError e

  InitGitError e ->
    renderGitError e

  InitCabalError e ->
    renderCabalError e

  InitSubmoduleError e ->
    renderSubmoduleError e

  InitInstallError e ->
    renderInstallError e

  InitParseError file err ->
    file <> ": parse error: " <> err

  InitCabalFileNotFound dir ->
    "Could not find cabal file in: " <> dir

------------------------------------------------------------------------

initialize :: Maybe Profiling -> EitherT InitError IO ()
initialize mprofiling = do
  firstEitherT InitGitError initSubmodules

  sandboxDir <- firstEitherT InitCabalError initSandbox
  let statePath = sandboxDir </> "mafia/state.json"

  liftIO (T.putStrLn "Checking for changes to dependencies...")
  previous <- readMafiaState statePath
  current  <- getMafiaState (mprofiling <|> fmap msProfiling previous)
  hasDist  <- liftIO $ doesDirectoryExist "dist"

  packages <- checkPackages

  let currentInstall  = msInstallState current
      previousInstall = fmap msInstallState previous
      needInstall     = previousInstall /= Just currentInstall || packages == PackagesBroken

  when needInstall $ do
    liftIO (T.putStrLn "Installing dependencies...")
    let sdeps = Set.toList (isSourceDependencies (msInstallState current))
    firstEitherT InitInstallError $ installDependencies sdeps

  let needConfigure = needInstall || previous /= Just current || not hasDist

  when needConfigure $ do
    firstEitherT InitCabalError . cabal_ "configure" $
      profilingArgs (msProfiling current) <>
      [ "--enable-tests"
      , "--enable-benchmarks" ]

  when (needInstall || needConfigure) $ do
    writeMafiaState statePath current

profilingArgs :: Profiling -> [Argument]
profilingArgs = \case
  DisableProfiling -> ["--disable-profiling"]
  EnableProfiling  -> ["--enable-profiling", "--ghc-options=-fprof-auto-top"]

------------------------------------------------------------------------

data PackageState =
    PackagesOk
  | PackagesBroken
    deriving (Eq, Ord, Show)

checkPackages :: EitherT InitError IO PackageState
checkPackages = do
  packageDB     <- firstEitherT InitCabalError getPackageDB
  packages      <- getDirectoryListing (RecursiveDepth 0) packageDB
  packagesExist <- and <$> traverse doesFileExist packages
  case packagesExist of
    True  -> return PackagesOk
    False -> return PackagesBroken

------------------------------------------------------------------------

data Profiling =
    DisableProfiling
  | EnableProfiling
    deriving (Eq, Ord, Show)

data InstallState =
  InstallState {
      isSourceDependencies :: Set SourcePackage
    , _isCabalFile         :: Hash
    } deriving (Eq, Ord, Show)

data MafiaState =
  MafiaState {
      msInstallState :: InstallState
    , msProfiling    :: Profiling
    } deriving (Eq, Ord, Show)

instance ToJSON Profiling where
  toJSON = \case
    DisableProfiling ->
      Bool False
    EnableProfiling ->
      Bool True

instance FromJSON Profiling where
  parseJSON = \case
    Bool False ->
      pure DisableProfiling
    Bool True ->
      pure EnableProfiling
    _ ->
      mzero

instance ToJSON InstallState where
  toJSON (InstallState sds cfh) =
    A.object
      [ "source-dependencies" .= sds
      , "cabal-file"          .= cfh ]

instance FromJSON InstallState where
  parseJSON = \case
    Object o ->
      InstallState <$>
        o .: "source-dependencies" <*>
        o .: "cabal-file"
    _ ->
      mzero

instance ToJSON MafiaState where
  toJSON (MafiaState is p) =
    A.object
      [ "install"   .= is
      , "profiling" .= p ]

instance FromJSON MafiaState where
  parseJSON = \case
    Object o ->
      MafiaState <$>
        o .: "install" <*>
        o .: "profiling"
    _ ->
      mzero

getMafiaState :: Maybe Profiling -> EitherT InitError IO MafiaState
getMafiaState mprofiling = do
  sdeps <- getSourceDependencies
  dir   <- getCurrentDirectory
  mfile <- getCabalFile dir
  case mfile of
    Nothing   -> left (InitCabalFileNotFound dir)
    Just file -> do
      hash  <- firstEitherT InitHashError (hashFile file)
      let profiling = fromMaybe DisableProfiling mprofiling
      return (MafiaState (InstallState sdeps hash) profiling)

getSourceDependencies :: EitherT InitError IO (Set SourcePackage)
getSourceDependencies = do
  sources <- Set.toList <$> firstEitherT InitSubmoduleError getSubmoduleSources
  Set.fromList . catMaybes <$> firstEitherT InitCabalError (mapConcurrentlyE getSourcePackage sources)

readMafiaState :: File -> EitherT InitError IO (Maybe MafiaState)
readMafiaState file = do
  mbs <- readBytes file
  case mbs of
    Nothing -> return Nothing
    Just bs ->
      fmap Just $
      hoistEither $
      first (InitParseError file . T.pack) $
      A.eitherDecodeStrict' bs

writeMafiaState :: MonadIO m => File -> MafiaState -> m ()
writeMafiaState file state = do
  createDirectoryIfMissing True (takeDirectory file)
  writeBytes file (L.toStrict (A.encode state))
