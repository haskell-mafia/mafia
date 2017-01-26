{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Mafia.Init (
    Profiling(..)
  , SourceVintage(..)

  , initialize
  , getSourceDependencies

  , readInstallConstraints

  , InitError(..)
  , renderInitError
  ) where

import           Control.Monad.IO.Class (MonadIO(..))

import           Data.Aeson (Value(..), ToJSON(..), FromJSON(..), (.:), (.=))
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Mafia.Cabal
import           Mafia.Git
import           Mafia.Hash
import           Mafia.IO
import           Mafia.Install
import           Mafia.Lock
import           Mafia.Path
import           Mafia.Process
import           Mafia.Submodule

import           P

import           System.IO (IO, stderr)

import           X.Control.Monad.Trans.Either (EitherT, runEitherT, hoistEither)


data InitError =
    InitHashError HashError
  | InitGitError GitError
  | InitCabalError CabalError
  | InitSubmoduleError SubmoduleError
  | InitInstallError InstallError
  | InitLockError LockError
  | InitParseError File Text
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

  InitLockError e ->
    renderLockError e

  InitParseError file err ->
    file <> ": parse error: " <> err

------------------------------------------------------------------------

initialize :: SourceVintage -> Maybe Profiling -> Maybe [Flag] -> EitherT InitError IO ()
initialize dfilter mprofiling mflags = do
  firstT InitGitError initSubmodules

  sandboxDir <- firstT InitCabalError initSandbox

  -- When switching between ghc versions, cabal-install 1.24 and later get
  -- confused about where the sandbox is supposed to be because they read the
  -- ghc version from 'dist/setup-config' rather than looking at the ghc on the
  -- PATH to detect the version.
  --
  -- We don't want to read the 'dist/setup-config' because the format is a
  -- binary blob that changes with every Cabal library release, so we detect
  -- this situation by running a benign command that uses the cabal sandbox and
  -- if it errors out then we delete 'dist/setup-config' and ensure that
  -- 'cabal configure' is re-run below.
  sandboxStatus <- liftIO checkSandbox

  when (sandboxStatus == SandboxKO) $
    ignoreIO $ removeFile "dist/setup-config"

  let statePath = sandboxDir </> "mafia/state." <> T.pack (show mafiaStateVersion) <> ".json"

  clearAddSourceDependencies sandboxDir

  defaultLockFile <- firstT InitLockError $ getLockFile =<< getCurrentDirectory
  lockFile <- fromMaybe defaultLockFile <$> lookupEnv "MAFIA_LOCK"

  previous <- readMafiaState statePath

  current <-
    maybe pure (tryKeepProjectSources dfilter) previous =<<
    getMafiaState (mprofiling <|> fmap msProfiling previous) (mflags <|> fmap msFlags previous) lockFile

  hasSetupConfig <- liftIO $ doesFileExist "dist/setup-config"

  packages <- checkPackages

  let needInstall = previous /= Just current || packages == PackagesBroken

  when needInstall $ do
    liftIO (T.hPutStrLn stderr "Installing dependencies...")

    let
      sdeps = Set.toList (msSourceDependencies current)
      flavours = profilingFlavour (msProfiling current)
      flags = msFlags current

    constraints <-
      if T.null lockFile then
        pure []
      else
        fmap (fromMaybe []) . firstT InitLockError $ readLockFile lockFile

    installed <- firstT InitInstallError $ installDependencies flavours flags sdeps constraints

    -- Note that we filter out source packages. Storing their version in the
    -- constraints file is redundant, as the accessible source code is the only
    -- version of that package which is ever available for installation.
    writeInstallConstraints .
      concatMap packageRefConstraints .
      filter (isNothing . refSrcPkg) .
      fmap pkgRef $
      Set.toList installed

  let needConfigure = needInstall || not hasSetupConfig

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
    , "--ghc-options=-rtsopts"
    ]

-- If a user or an older version of mafia has used 'cabal sandbox add-source'
-- then some source dependencies can get installed twice unecessarily. This
-- removes all the add-source dependencies by creating an empty index file.
clearAddSourceDependencies :: SandboxDir -> EitherT InitError IO ()
clearAddSourceDependencies sandboxDir = do
  deps <- firstT InitCabalError $ readIndexFile sandboxDir

  for_ deps $ \dep -> do
    dep' <- fromMaybe dep <$> makeRelativeToCurrentDirectory dep
    liftIO . T.hPutStrLn stderr $ "Removing add-source dependency: " <> dep'

  firstT InitCabalError $ createIndexFile [] sandboxDir

------------------------------------------------------------------------

data SandboxStatus =
    SandboxOK
  | SandboxKO
    deriving (Eq, Ord, Show)

checkSandbox :: IO SandboxStatus
checkSandbox = do
  e <- runEitherT $ cabal "sandbox" ["hc-pkg", "list"]
  case e of
    Left _ ->
      pure SandboxKO
    Right Hush ->
      pure SandboxOK

------------------------------------------------------------------------

getInstallConstraintsFile :: EitherT InitError IO File
getInstallConstraintsFile = do
  sandboxDir <- firstT InitCabalError initSandbox
  pure $ sandboxDir </> "mafia/install.constraints"

readInstallConstraints :: EitherT InitError IO (Maybe [Constraint])
readInstallConstraints = do
  file <- getInstallConstraintsFile
  mtxt <- readUtf8 file
  case mtxt of
    Nothing ->
      pure Nothing
    Just txt ->
      fmap Just .
      traverse (hoistEither . first InitCabalError . parseConstraint) $
      T.lines txt

writeInstallConstraints :: [Constraint] -> EitherT InitError IO ()
writeInstallConstraints xs = do
  file <- getInstallConstraintsFile
  createDirectoryIfMissing True (takeDirectory file)
  writeUtf8 file . T.unlines $ fmap renderConstraint xs

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

data SourceVintage =
    LatestSources
  | PermitStaleProjectSources
    deriving (Eq, Ord, Show)

data MafiaState =
  MafiaState {
      msSourceDependencies :: Set SourcePackage
    , _msCabalFile :: Hash
    , _msLockFile :: Maybe Hash
    , msProfiling :: Profiling
    , msFlags :: [Flag]
    } deriving (Eq, Ord, Show)

mafiaStateVersion :: Int
mafiaStateVersion = 2

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
  toJSON (MafiaState sds cfh lfh p fs) =
    A.object
      [ "source-dependencies" .= sds
      , "cabal-file" .= cfh
      , "lock-file" .= lfh
      , "profiling" .= p
      , "flags" .= fmap renderFlag fs ]

instance FromJSON MafiaState where
  parseJSON = \case
    Object o ->
      MafiaState <$>
        o .: "source-dependencies" <*>
        o .: "cabal-file" <*>
        o .: "lock-file" <*>
        o .: "profiling" <*>
        (o .: "flags" >>= parseFlags)
    _ ->
      mzero

parseFlags :: Alternative f => [Text] -> f [Flag]
parseFlags =
  traverse (maybe empty pure . parseFlag)

tryKeepProjectSources :: SourceVintage -> MafiaState -> MafiaState -> EitherT InitError IO MafiaState
tryKeepProjectSources vintage previousState currentState =
  case vintage of
    LatestSources ->
      pure currentState
    PermitStaleProjectSources -> do
      project <- Map.fromSet (const ()) <$> firstT InitGitError getProjectSources

      let
        fromSet =
          Map.fromList .
          fmap (\x -> (spDirectory x, x)) .
          Set.toList

        previous =
          fromSet $ msSourceDependencies previousState

        -- reuse the project source dependencies from the previous state
        reuse =
          Map.intersection previous project

        current =
          Map.union reuse . fromSet $
          msSourceDependencies currentState

      pure $ currentState {
          msSourceDependencies = Set.fromList $ Map.elems current
        }

getMafiaState :: Maybe Profiling -> Maybe [Flag] -> File -> EitherT InitError IO MafiaState
getMafiaState mprofiling mflags lockFile = do
  sdeps <- getSourceDependencies
  dir <- getCurrentDirectory
  chash <- getCabalFileHash dir
  lhash <- firstT InitHashError $ tryHashFile lockFile

  let
    profiling = fromMaybe DisableProfiling mprofiling
    flags = fromMaybe [] mflags

  return (MafiaState sdeps chash lhash profiling flags)

getCabalFileHash :: Directory -> EitherT InitError IO Hash
getCabalFileHash dir = do
  file <- firstT InitCabalError $ getCabalFile dir
  firstT InitHashError (hashFile file)

getSourceDependencies :: EitherT InitError IO (Set SourcePackage)
getSourceDependencies =
  getSourcePackages =<< firstT InitSubmoduleError getAvailableSources

getSourcePackages :: Set Directory -> EitherT InitError IO (Set SourcePackage)
getSourcePackages =
  fmap (Set.fromList . catMaybes) .
  firstT InitCabalError .
  mapConcurrentlyE getSourcePackage .
  Set.toList

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
