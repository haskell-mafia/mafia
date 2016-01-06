{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Mafia.Install
  ( InstallError(..)
  , renderInstallError
  , installDependencies
  ) where

import           Control.Exception (SomeException, IOException)
import           Control.Monad.Catch (MonadCatch, handle)
import           Control.Monad.IO.Class (MonadIO(..))

import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

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
  | InstallDisaster SomeException
    deriving (Show)

renderInstallError :: InstallError -> Text
renderInstallError = \case
  InstallGhcError e ->
    renderGhcError e

  InstallCabalError e ->
    renderCabalError e

  InstallDisaster ex ->
    "Disaster: " <> T.pack (show ex)

------------------------------------------------------------------------

installDependencies :: [SourcePackage] -> EitherT InstallError IO ()
installDependencies spkgs = do
  globals   <- installGlobalDependencies spkgs
  _         <- firstEitherT InstallCabalError initSandbox
  packageDB <- firstEitherT InstallCabalError getPackageDB

  ignoreIO $ removeDirectoryRecursive packageDB
  createDirectoryIfMissing True packageDB

  -- create symlinks to the relevant package .conf files in the
  -- package-db, then call recache so that ghc is aware of them.
  env  <- getPackageEnv
  mapM_ (link packageDB env) globals
  Hush <- firstEitherT InstallCabalError $ cabal "sandbox" ["hc-pkg", "recache"]

  return ()

installGlobalDependencies :: [SourcePackage] -> EitherT InstallError IO [Package]
installGlobalDependencies spkgs = do
  deps <- firstEitherT InstallCabalError (findDependencies spkgs)
  let producer q = mapM_ (writeQueue q) deps

  nprocs <- liftIO getNumProcessors
  env    <- getPackageEnv
  firstEitherT squashRunError $ consume_ producer nprocs (install env)

  return deps

------------------------------------------------------------------------

link :: Directory -> PackageEnv -> Package -> EitherT InstallError IO ()
link db env p@(Package (PackageRef pid _ _) _ _) = do
  let pcfg = packageConfigPath env p
  unlessM (doesFileExist pcfg) (install env p)

  let dest = db </> renderPackageId pid <> ".conf"
  liftIO $ createSymbolicLink (T.unpack pcfg) (T.unpack dest)

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
install :: PackageEnv -> Package -> EitherT InstallError IO ()
install env p@(Package (PackageRef pid _ msrc) deps _) = do
  let packageCfg = packageConfigPath env p
  unlessM (doesFileExist packageCfg) $ do
    withPackageLock env p $ do
      unlessM (doesFileExist packageCfg) $ do
        liftIO $ T.putStrLn ("Building " <> renderHashId p)

        let dir = packageSandboxPath env p
        ignoreIO (removeDirectoryRecursive dir)
        createDirectoryIfMissing True dir

        let sandboxCfg = dir </> "sandbox.config"
            sbcabal x xs = firstEitherT InstallCabalError $ cabalFrom dir (Just sandboxCfg) x xs

        Hush <- sbcabal "sandbox" ["init", "--sandbox", dir]

        case msrc of
          Nothing  -> return () -- hackage package
          Just src -> do
            Hush <- sbcabal "sandbox" ["add-source", spDirectory src]
            return ()

        db <- firstEitherT InstallCabalError (readPackageDB sandboxCfg)

        -- create symlinks to the relevant package .conf files in the
        -- package-db, then call recache so that ghc is aware of them.
        mapM_ (link db env) (Set.toList (transitiveOfPackages deps))
        Hush <- sbcabal "sandbox" ["hc-pkg", "recache"]

        let constraints = concatMap (\c -> ["--constraint", c]) (constraintsOfPackage p)
        Pass <- sbcabal "install" $ [ "--ghc-options=-j"
                                    , "--ghc-options=-fprof-auto-exported"
                                    , "--enable-library-profiling"
                                    , "--enable-documentation"
                                    , "--haddock-hoogle"
                                    , "--haddock-hyperlink-source"
                                    , "--max-backjumps=0"
                                    , renderPackageId pid
                                    ] <> constraints

        Out out <- sbcabal "sandbox" ["hc-pkg", "--", "describe", renderPackageId pid]

        writeUtf8 packageCfg out

ignoreIO :: MonadCatch m => m () -> m ()
ignoreIO =
  handle (\(_ :: IOException) -> return ())

squashRunError :: RunError InstallError -> InstallError
squashRunError = \case
  WorkerError e -> e
  BlowUpError e -> InstallDisaster e

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
envPackageCacheVersion = 0

envPackageCache :: PackageEnv -> Directory
envPackageCache env =
  envMafiaHome env </> "packages" </> T.pack (show envPackageCacheVersion) </> envGhcVersion env

getPackageEnv :: EitherT InstallError IO PackageEnv
getPackageEnv = do
  ghc  <- firstEitherT InstallGhcError getGhcVersion
  home <- getMafiaHome
  return (PackageEnv ghc home)

withPackageLock :: PackageEnv -> Package -> EitherT InstallError IO a -> EitherT InstallError IO a
withPackageLock env p io =
  bracketEitherT' (lockPackage env p) (liftIO . unlockFile) (const io)

lockPackage :: PackageEnv -> Package -> EitherT InstallError IO FileLock
lockPackage env p = do
  let path = packageLockPath env p
  ignoreIO $ createDirectoryIfMissing True (takeDirectory path)
  mlock <- liftIO $ tryLockFile (T.unpack path) Exclusive
  case mlock of
    Just lock -> return lock
    Nothing   -> liftIO $ do
      T.putStrLn ("Waiting for " <> renderHashId p)
      lockFile (T.unpack path) Exclusive

packageConfigPath :: PackageEnv -> Package -> File
packageConfigPath env p =
  packageSandboxPath env p </> "package.conf"

packageLockPath :: PackageEnv -> Package -> File
packageLockPath env p =
  envPackageCache env </> ".locks" </> renderHashId p

packageSandboxPath :: PackageEnv -> Package -> Directory
packageSandboxPath env p = do
  envPackageCache env </> renderHashId p
