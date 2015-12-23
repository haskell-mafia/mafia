{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Mafia.Init
  ( initialize

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
  | InitParseError Text
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

  InitParseError err ->
    "Parse error: " <> err

  InitCabalFileNotFound dir ->
    "Could not find cabal file in: " <> dir

------------------------------------------------------------------------

initialize :: EitherT InitError IO ()
initialize = do
  firstEitherT InitGitError initSubmodules

  cacheDir <- getCacheDir
  let statePath = cacheDir </> "state.json"

  liftIO (T.putStrLn "Checking for changes to dependencies...")
  previous <- readMafiaState statePath
  current  <- getMafiaState
  hasDist  <- liftIO $ doesDirectoryExist "dist"

  when (previous /= Just current || not hasDist) $ do
    liftIO (T.putStrLn "Installing dependencies...")
    let sdeps = Set.toList (msSourceDependencies current)
    firstEitherT InitInstallError $ installDependencies sdeps
    firstEitherT InitCabalError $ cabal_ "configure" ["--enable-tests" , "--enable-benchmarks"]
    writeMafiaState statePath current

------------------------------------------------------------------------

data MafiaState =
  MafiaState {
      msSourceDependencies :: Set SourcePackage
    , _msCabalFile          :: Hash
    } deriving (Eq, Ord, Show)

instance ToJSON MafiaState where
  toJSON (MafiaState sds cfh) =
    A.object
      [ "source-dependencies" .= sds
      , "cabal-file"          .= cfh ]

instance FromJSON MafiaState where
  parseJSON = \case
    Object o ->
      MafiaState <$>
        o .: "source-dependencies" <*>
        o .: "cabal-file"
    _ ->
      mzero

getMafiaState :: EitherT InitError IO MafiaState
getMafiaState = do
  sdeps <- getSourceDependencies
  dir   <- getCurrentDirectory
  mfile <- getCabalFile dir
  case mfile of
    Nothing   -> left (InitCabalFileNotFound dir)
    Just file -> do
      hash  <- firstEitherT InitHashError (hashFile file)
      return (MafiaState sdeps hash)

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
      first (InitParseError . T.pack) $
      A.eitherDecodeStrict' bs

writeMafiaState :: MonadIO m => File -> MafiaState -> m ()
writeMafiaState file state = do
  writeBytes file (L.toStrict (A.encode state))

getCacheDir :: EitherT InitError IO Directory
getCacheDir = do
  cacheDir <- (</> "mafia") <$> firstEitherT InitCabalError initSandbox
  createDirectoryIfMissing False cacheDir
  return cacheDir
