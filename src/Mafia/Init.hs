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

import           X.Control.Monad.Trans.Either (EitherT, left, hoistEither)


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

initialize :: Maybe Profiling -> Maybe [Flag] -> EitherT InitError IO ()
initialize mprofiling mflags = do
  firstT InitGitError initSubmodules

  sandboxDir <- firstT InitCabalError initSandbox
  let statePath = sandboxDir </> "mafia/state." <> T.pack (show mafiaStateVersion) <> ".json"

  clearAddSourceDependencies sandboxDir

  liftIO (T.putStrLn "Checking for changes to dependencies...")
  previous <- readMafiaState statePath
  current <- getMafiaState (mprofiling <|> fmap msProfiling previous) (mflags <|> fmap msFlags previous)
  hasDist <- liftIO $ doesDirectoryExist "dist"

  packages <- checkPackages

  let needInstall = previous /= Just current || packages == PackagesBroken

  when needInstall $ do
    liftIO (T.putStrLn "Installing dependencies...")
    let sdeps    = Set.toList (msSourceDependencies current)
        flavours = profilingFlavour (msProfiling current)
    firstT InitInstallError $ installDependencies flavours sdeps

  let needConfigure = needInstall || not hasDist

  when needConfigure $ do
    firstT InitCabalError . cabal_ "configure" $
      profilingArgs (msProfiling current) <>
      fmap flagArg (msFlags current) <>
      [ "--enable-tests"
      , "--enable-benchmarks" ]

  when (needInstall || needConfigure) $ do
    writeMafiaState statePath current

profilingFlavour :: Profiling -> Flavour
profilingFlavour = \case
  DisableProfiling -> Vanilla
  EnableProfiling  -> Profiling

profilingArgs :: Profiling -> [Argument]
profilingArgs = \case
  DisableProfiling -> [
      "--disable-profiling"
    , "--enable-executable-stripping"
    ]
  EnableProfiling  -> [
      "--enable-profiling"
    , "--disable-executable-stripping"
    , "--ghc-options=-fprof-auto-top"
    ]

flagArg :: Flag -> Argument
flagArg = \case
  FlagOff f ->
    "--flags=-" <> f
  FlagOn f ->
    "--flags=" <> f

-- If a user or an older version of mafia has used 'cabal sandbox add-source'
-- then some source dependencies can get installed twice unecessarily. This
-- removes all the add-source dependencies by creating an empty index file.
clearAddSourceDependencies :: SandboxDir -> EitherT InitError IO ()
clearAddSourceDependencies sandboxDir = do
  deps <- firstT InitCabalError $ readIndexFile sandboxDir

  for_ deps $ \dep -> do
    dep' <- fromMaybe dep <$> makeRelativeToCurrentDirectory dep
    liftIO . T.putStrLn $ "Removing add-source dependency: " <> dep'

  firstT InitCabalError $ createIndexFile [] sandboxDir

------------------------------------------------------------------------

data PackageState =
    PackagesOk
  | PackagesBroken
    deriving (Eq, Ord, Show)

checkPackages :: EitherT InitError IO PackageState
checkPackages = do
  packageDB     <- firstT InitCabalError getPackageDB
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

data MafiaState =
  MafiaState {
      msSourceDependencies :: Set SourcePackage
    , _msCabalFile :: Hash
    , msProfiling :: Profiling
    , msFlags :: [Flag]
    } deriving (Eq, Ord, Show)

mafiaStateVersion :: Int
mafiaStateVersion = 1

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

instance ToJSON MafiaState where
  toJSON (MafiaState sds cfh p fs) =
    A.object
      [ "source-dependencies" .= sds
      , "cabal-file" .= cfh
      , "profiling" .= p
      , "flags" .= fmap renderFlag fs ]

instance FromJSON MafiaState where
  parseJSON = \case
    Object o ->
      MafiaState <$>
        o .: "source-dependencies" <*>
        o .: "cabal-file" <*>
        o .: "profiling" <*>
        (o .: "flags" >>= parseFlags)
    _ ->
      mzero

parseFlags :: Alternative f => [Text] -> f [Flag]
parseFlags =
  traverse (maybe empty pure . parseFlag)

getMafiaState :: Maybe Profiling -> Maybe [Flag] -> EitherT InitError IO MafiaState
getMafiaState mprofiling mflags = do
  sdeps <- getSourceDependencies
  dir   <- getCurrentDirectory
  mfile <- getCabalFile dir
  case mfile of
    Nothing   -> left (InitCabalFileNotFound dir)
    Just file -> do
      hash  <- firstT InitHashError (hashFile file)
      let
        profiling = fromMaybe DisableProfiling mprofiling
        flags = fromMaybe [] mflags
      return (MafiaState sdeps hash profiling flags)

getSourceDependencies :: EitherT InitError IO (Set SourcePackage)
getSourceDependencies = do
  sources <- Set.toList <$> firstT InitSubmoduleError getSubmoduleSources
  Set.fromList . catMaybes <$> firstT InitCabalError (mapConcurrentlyE getSourcePackage sources)

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
