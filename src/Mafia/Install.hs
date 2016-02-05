{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Mafia.Install
  ( Flavour(..)
  , installDependencies
  , transitiveOfPackages

  , InstallError(..)
  , renderInstallError
  ) where

import           Control.Exception (SomeException)
import           Control.Monad.IO.Class (MonadIO(..))

import qualified Data.ByteString as B
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Read as T

import           GHC.Conc (getNumProcessors)

import           Mafia.Cabal.Dependencies
import           Mafia.Cabal.Package
import           Mafia.Cabal.Process
import           Mafia.Cabal.Sandbox
import           Mafia.Cabal.Types
import           Mafia.Ghc
import           Mafia.Home
import           Mafia.IO
import           Mafia.Package
import           Mafia.Path
import           Mafia.Process
import           Mafia.Tree

import           P

import           System.FileLock (SharedExclusive(..), FileLock)
import           System.FileLock (lockFile, tryLockFile, unlockFile)
import           System.IO (IO)
import           System.Posix.Files (createSymbolicLink)

import           Twine.Parallel (RunError(..), consume_)
import           Twine.Data.Queue  (writeQueue)

import           X.Control.Monad.Trans.Either

------------------------------------------------------------------------

data InstallError =
    InstallGhcError GhcError
  | InstallCabalError CabalError
  | InstallPackageError PackageId [Package] CabalError
  | InstallLinkError Package File
  | InstallEnvParseError EnvKey EnvValue Text
  | InstallDisaster SomeException
    deriving (Show)

renderInstallError :: InstallError -> Text
renderInstallError = \case
  InstallGhcError e ->
    renderGhcError e

  InstallCabalError e ->
    renderCabalError e

  InstallPackageError pid pkgs e ->
    "Failed to install " <> renderPackageId pid <> "\n" <>
    renderCabalError e <> "\n" <>
    TL.toStrict (TL.strip . renderTree $ filterPackages (pkgName pid) pkgs)

  InstallLinkError p pcfg ->
    "Failed to create symlink for " <> renderHashId p <> ", package config did not exist: " <> pcfg

  InstallEnvParseError key val err ->
    "Failed parsing environment variable " <> key <> "=" <> val <> " (" <> err <> ")"

  InstallDisaster ex ->
    "Disaster: " <> T.pack (show ex)

------------------------------------------------------------------------

installDependencies :: Flavour -> [Flag] -> [SourcePackage] -> EitherT InstallError IO ()
installDependencies flavour flags spkgs = do
  globals   <- installGlobalDependencies flavour flags spkgs
  _         <- firstT InstallCabalError initSandbox
  packageDB <- firstT InstallCabalError getPackageDB

  ignoreIO $ removeDirectoryRecursive packageDB
  createDirectoryIfMissing True packageDB

  -- create symlinks to the relevant package .conf files in the
  -- package-db, then call recache so that ghc is aware of them.
  env <- getPackageEnv
  mapM_ (link packageDB env) globals
  Hush <- firstT InstallCabalError $ cabal "sandbox" ["hc-pkg", "recache"]

  return ()

installGlobalDependencies :: Flavour -> [Flag] -> [SourcePackage] -> EitherT InstallError IO [Package]
installGlobalDependencies flavour flags spkgs = do
  deps <- firstT InstallCabalError (findDependencies flags spkgs)
  let producer q = mapM_ (writeQueue q) deps

  mw  <- getMafiaWorkers
  gw  <- getGhcWorkers
  env <- getPackageEnv
  firstT (squashRunError deps) $ consume_ producer mw (install gw env flavour)

  return . Set.toList $ transitiveOfPackages deps

------------------------------------------------------------------------

type NumWorkers = Int

getDefaultWorkers :: MonadIO m => m NumWorkers
getDefaultWorkers =
  min 4 `liftM` liftIO getNumProcessors

getMafiaWorkers :: EitherT InstallError IO NumWorkers
getMafiaWorkers = do
  def <- getDefaultWorkers
  fromMaybe def <$> lookupPositive "MAFIA_WORKERS"

getGhcWorkers :: EitherT InstallError IO NumWorkers
getGhcWorkers = do
  def <- getDefaultWorkers
  fromMaybe def <$> lookupPositive "MAFIA_GHC_WORKERS"

lookupPositive :: Text -> EitherT InstallError IO (Maybe Int)
lookupPositive key = do
  mtxt <- lookupEnv key
  case mtxt of
    Nothing ->
      return Nothing
    Just txt ->
      case T.decimal txt of
        Right (x, "") | x > 0 ->
          return (Just x)
        Right _ ->
          left (InstallEnvParseError key txt "not a positive number")
        Left str ->
          left (InstallEnvParseError key txt (T.pack str))

------------------------------------------------------------------------

data Flavour =
    Vanilla
  | Profiling
  | Documentation
    deriving (Eq, Ord, Show)

renderFlavour :: Flavour -> Text
renderFlavour = \case
  Vanilla       -> "vanilla"
  Profiling     -> "profiling"
  Documentation -> "documentation"

renderFlavourSuffix :: Flavour -> Text
renderFlavourSuffix = \case
  Vanilla       -> ""
  Profiling     -> " [profiling]"
  Documentation -> " [documentation]"

------------------------------------------------------------------------

-- | Installs a package and its dependencies in to the mafia global package
--   cache in $MAFIA_HOME by taking the following steps:
--
--   1. Checks if the package.conf file already exists, this is what decides
--      whether we've already installed a package or not.
--
--   2. Takes a machine wide lock on the package to make sure that two mafia's
--      can still run at the same time.
--
--   3. Creates a fresh a sandbox in the global cache.
--
--   4. Adds the source if the package is a source/submodule dependency.
--
--   5. Registers any dependencies in to the sandbox package db by creating
--      symbolic links.
--
--   6. Install the package.
--
--   7. Create a package.conf file which can be symlinked in to other package db's.
--
install :: NumWorkers -> PackageEnv -> Flavour -> Package -> EitherT InstallError IO ()
install w penv flavour p@(Package (PackageRef pid _ _) deps _) = do
  -- only try to install this package/flavour if we haven't done it already.
  -- it's important to do this before we do the same for any dependencies
  -- otherwise we end up doing an exponential number of checks.
  let fmark = packageFlavourMarker penv p flavour
  unlessM (doesFileExist fmark) $ do

    -- install package dependencies
    mapM_ (install w penv flavour) (Set.toList (transitiveOfPackages deps))

    -- the vanilla flavour must always be installed first:
    --  + it creates and sets up the package's sandbox
    --  + profiling builds need the vanilla build for template haskell to run
    --  + documentation builds need the vanilla build to harvest the .hi files
    when (flavour /= Vanilla) $
      install w penv Vanilla p

    -- we take this lock *after* all the package dependencies have been
    -- installed, otherwise we can prevent some parallelism from occurring
    unlessM (doesFileExist fmark) $
      withPackageLock penv p flavour $
        unlessM (doesFileExist fmark) $ do
          when (flavour == Vanilla) $
            createPackageSandbox penv p

          let sbdir = packageSandboxDir penv p
              sbcfg = packageSandboxConfig penv p

          let sbcabal x xs =
                firstT (InstallPackageError pid []) $ cabalFrom sbdir (Just sbcfg) x xs

          liftIO . T.putStrLn $ "Building " <> renderHashId p <> renderFlavourSuffix flavour

          Pass <- sbcabal "install" $
            [ "--ghc-options=-j" <> T.pack (show w)
            , "--max-backjumps=0"
            , renderPackageId pid ] <>
            flavourArgs flavour <>
            concatMap (\c -> ["--constraint", c]) (constraintsOfPackage p)

          when (flavour == Vanilla) $ do
            Out out <- sbcabal "sandbox" ["hc-pkg", "--", "describe", renderPackageId pid]
            writeUtf8 (packageConfig penv p) out

          writeBytes fmark B.empty

flavourArgs :: Flavour -> [Argument]
flavourArgs = \case
  Vanilla ->
    []
  Profiling ->
    [ "--reinstall"
    , "--ghc-options=-fprof-auto-exported"
    , "--enable-library-profiling" ]
  Documentation ->
    [ "--reinstall"
    , "--enable-documentation"
    , "--haddock-hoogle"
    , "--haddock-hyperlink-source" ]

-- | Creates and installs/links the dependencies for a package in to its well
--   known global sandbox.
createPackageSandbox :: PackageEnv -> Package -> EitherT InstallError IO ()
createPackageSandbox penv p@(Package (PackageRef pid _ msrc) deps _) = do
  liftIO . T.putStrLn $ "Creating sandbox for " <> renderHashId p

  let sbdir = packageSandboxDir penv p
      sbcfg = packageSandboxConfig penv p
      sbsrc = packageSourceDir penv p

  let sbcabal x xs =
        firstT InstallCabalError $ cabalFrom sbdir (Just sbcfg) x xs

  ignoreIO (removeDirectoryRecursive sbdir)
  createDirectoryIfMissing True sbdir

  Hush <- sbcabal "sandbox" ["init", "--sandbox", sbdir]

  case msrc of
    -- hackage package
    Nothing -> do
      -- We install hackage packages by unpacking them in to a src/ directory
      -- inside the package's location in the global cache. This allows us to
      -- cheaply upgrade non-profiling builds to profiling builds as the .o
      -- files are kept around in the dist/ directory. It also has the benefit
      -- of not polluting the $TMPDIR on the build bot.
      createDirectoryIfMissing True sbsrc
      Hush <- sbcabal "unpack" ["--destdir=" <> sbsrc, renderPackageId pid]
      Hush <- sbcabal "sandbox" ["add-source", sbsrc </> renderPackageId pid]
      return ()

    -- source package
    Just src -> do
      Hush <- sbcabal "sandbox" ["add-source", spDirectory src]
      return ()

  db <- firstT InstallCabalError (readPackageDB sbcfg)

  -- create symlinks to the relevant package .conf files in the
  -- package-db, then call recache so that ghc is aware of them.
  mapM_ (link db penv) (Set.toList (transitiveOfPackages deps))
  Hush <- sbcabal "sandbox" ["hc-pkg", "recache"]

  return ()

link :: Directory -> PackageEnv -> Package -> EitherT InstallError IO ()
link db env p@(Package (PackageRef pid _ _) _ _) = do
  let pcfg = packageConfig env p
  unlessM (doesFileExist pcfg) $
    left (InstallLinkError p pcfg)

  let dest = db </> renderPackageId pid <> ".conf"
  liftIO $ createSymbolicLink (T.unpack pcfg) (T.unpack dest)

squashRunError :: [Package] -> RunError InstallError -> InstallError
squashRunError pkgs = \case
  WorkerError (InstallPackageError pid [] e) ->
    InstallPackageError pid pkgs e
  WorkerError e ->
    e
  BlowUpError e ->
    InstallDisaster e

------------------------------------------------------------------------

constraintsOfPackage :: Package -> [Text]
constraintsOfPackage p =
  let xs = Set.toList (transitiveOfPackages [p])
  in concatMap (constraintsOfRef . pkgRef) xs

constraintsOfRef :: PackageRef -> [Text]
constraintsOfRef (PackageRef pid fs _) =
  [ nameOfPackage pid <> " == " <> versionOfPackage pid ] <>
  fmap (\x -> nameOfPackage pid <> " " <> renderFlag x) fs

transitiveOfPackages :: [Package] -> Set Package
transitiveOfPackages deps =
  Set.unions (Set.fromList deps : fmap (transitiveOfPackages . pkgDeps) deps)

versionOfPackage :: PackageId -> Text
versionOfPackage =
  renderVersion . pkgVersion

nameOfPackage :: PackageId -> Text
nameOfPackage =
  unPackageName . pkgName

------------------------------------------------------------------------

data PackageEnv =
  PackageEnv {
      envGhcVersion :: GhcVersion
    , envMafiaHome  :: Directory
    } deriving (Eq, Ord, Show)

-- the package cache path includes a version number in case the contents or
-- layout of the cache changes in subsequent mafia versions.
envPackageCacheVersion :: Int
envPackageCacheVersion = 1

envPackageCache :: PackageEnv -> Directory
envPackageCache env =
  envMafiaHome env </> "packages" </> T.pack (show envPackageCacheVersion) </> envGhcVersion env

getPackageEnv :: EitherT InstallError IO PackageEnv
getPackageEnv = do
  ghc  <- firstT InstallGhcError getGhcVersion
  home <- getMafiaHome
  return (PackageEnv ghc home)

withPackageLock :: PackageEnv -> Package -> Flavour -> EitherT InstallError IO a -> EitherT InstallError IO a
withPackageLock env p f io =
  bracketEitherT' (lockPackage env p f) (liftIO . unlockFile) (const io)

lockPackage :: PackageEnv -> Package -> Flavour -> EitherT InstallError IO FileLock
lockPackage env p f = do
  let path = packageLockPath env p
  ignoreIO $ createDirectoryIfMissing True (takeDirectory path)
  mlock <- liftIO $ tryLockFile (T.unpack path) Exclusive
  case mlock of
    Just lock -> return lock
    Nothing   -> liftIO $ do
      T.putStrLn ("Waiting for " <> renderHashId p <> renderFlavourSuffix f)
      lockFile (T.unpack path) Exclusive

packageConfig :: PackageEnv -> Package -> File
packageConfig env p =
  packageSandboxDir env p </> "package.conf"

packageFlavourMarker :: PackageEnv -> Package -> Flavour -> File
packageFlavourMarker env p flav =
  packageSandboxDir env p </> "package." <> renderFlavour flav

packageLockPath :: PackageEnv -> Package -> File
packageLockPath env p =
  envPackageCache env </> ".locks" </> renderHashId p

packageSandboxDir :: PackageEnv -> Package -> SandboxDir
packageSandboxDir env p = do
  envPackageCache env </> renderHashId p

packageSandboxConfig :: PackageEnv -> Package -> SandboxConfigFile
packageSandboxConfig env p = do
  packageSandboxDir env p </> "sandbox.config"

packageSourceDir :: PackageEnv -> Package -> Directory
packageSourceDir env p = do
  packageSandboxDir env p </> "src"
