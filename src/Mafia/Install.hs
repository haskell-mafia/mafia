{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Mafia.Install
  ( Flavour(..)
  , installDependencies
  , installPackage
  , transitiveOfPackages

  , PackageEnv(..)
  , getPackageEnv
  , packageSandboxDir

  , InstallError(..)
  , renderInstallError
  ) where

import           Control.Exception (SomeException)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Parallel.Strategies (rpar, parMap)

import           Data.Bits (Bits(..))
import qualified Data.ByteString as B
import           Data.Char (ord)
import qualified Data.List as List
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Read as T
import           Data.Word (Word32)

import           GHC.Conc (getNumProcessors)

import           Mafia.Cabal.Constraint
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

import           Numeric (showHex)

import           P

import           System.FileLock (SharedExclusive(..), FileLock)
import           System.FileLock (lockFile, tryLockFile, unlockFile)
import           System.IO (IO, stderr)
import           System.Posix.Files (createSymbolicLink)

import           Twine.Parallel (RunError(..), consume_)
import           Twine.Data.Queue  (writeQueue)

import           X.Control.Monad.Trans.Either

------------------------------------------------------------------------

data InstallError =
    InstallGhcError GhcError
  | InstallCabalError CabalError
  | InstallPackageError PackageId (Set Package) CabalError
  | InstallLinkError Package File
  | InstallEnvParseError EnvKey EnvValue Text
  | InstallPlanPackageMissing PackageName
  | InstallPlanPackageDuplicate PackageName [Package]
  | InstallUnknownPackageType PackageId
  | InstallDisaster SomeException
    deriving (Show)

renderInstallError :: InstallError -> Text
renderInstallError = \case
  InstallGhcError e ->
    renderGhcError e

  InstallCabalError e ->
    renderCabalError e

  InstallPackageError pid@(PackageId name _) pkgs e ->
    "Failed to install " <> renderPackageId pid <> "\n" <>
    renderCabalError e <> "\n" <>
    let
      take n txt =
        let
          reasonable =
            List.take (n+1) $ TL.lines txt

          pname =
            TL.fromStrict (unPackageName name)

          presentable =
            if length reasonable > n then
              List.take n reasonable <>
              [ "── snip ──"
              , "Run 'mafia depends " <> pname <> " --tree' to see the full tree." ]
            else
              reasonable
        in
          TL.toStrict . TL.strip $ TL.unlines presentable
    in
      take 50 . renderTree $ filterPackages (pkgName pid) pkgs

  InstallLinkError p pcfg ->
    "Failed to create symlink for " <> renderHashId p <> ", package config did not exist: " <> pcfg

  InstallEnvParseError key val err ->
    "Failed parsing environment variable " <> key <> "=" <> val <> " (" <> err <> ")"

  InstallPlanPackageMissing name ->
    "Package not found in install plan: " <> unPackageName name

  InstallPlanPackageDuplicate name _ ->
    "Package duplicated in install plan: " <> unPackageName name

  InstallUnknownPackageType pid ->
    "Unknown package type for: " <> renderPackageId pid

  InstallDisaster ex ->
    "Disaster: " <> T.pack (show ex)

------------------------------------------------------------------------

installDependencies :: Flavour -> [Flag] -> [SourcePackage] -> [Constraint] -> EitherT InstallError IO (Set Package)
installDependencies flavour flags spkgs constraints = do
  pkg <- firstT InstallCabalError $ findDependenciesForCurrentDirectory flags spkgs constraints

  let
    tdeps = transitiveOfPackages (pkgDeps pkg)

  installGlobalPackages flavour tdeps

  _ <- firstT InstallCabalError initSandbox
  packageDB <- firstT InstallCabalError getPackageDB

  ignoreIO $ removeDirectoryRecursive packageDB
  createDirectoryIfMissing True packageDB

  -- create symlinks to the relevant package .conf files in the
  -- package-db, then call recache so that ghc is aware of them.
  env <- getPackageEnv
  mapM_ (link packageDB env) tdeps
  Hush <- firstT InstallCabalError $ cabal "sandbox" ["hc-pkg", "recache"]

  return tdeps

installPackage :: PackageName -> Maybe Version -> EitherT InstallError IO Package
installPackage name mver = do
  pkg <- firstT InstallCabalError $ findDependenciesForPackage name mver
  installGlobalPackages Vanilla (transitiveOfPackages (Set.singleton pkg))
  return pkg

installGlobalPackages :: Flavour -> Set Package -> EitherT InstallError IO ()
installGlobalPackages flavour deps = do
  let producer q = mapM_ (writeQueue q) deps

  mw  <- getMafiaWorkers
  gw  <- getGhcWorkers
  env <- getPackageEnv
  firstT (squashRunError deps) $ consume_ producer mw (install gw env flavour)

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
    mapM_ (install w penv flavour) (transitiveOfPackages deps)

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
          -- when we create the package sandbox it determines whether the
          -- package contains only executables, or is also available as a
          -- library, this is the package type.
          ptype <-
            if (flavour == Vanilla) then
              Just <$> createPackageSandbox penv p
            else
              pure Nothing

          let sbdir = packageSandboxDir penv p
              sbcfg = packageSandboxConfig penv p

          let sbcabal x xs =
                firstT (InstallPackageError pid Set.empty) $ cabalFrom sbdir (Just sbcfg) x xs

          liftIO . T.hPutStrLn stderr $ "Building " <> renderHashId p <> renderFlavourSuffix flavour

          PassErr <- sbcabal "install" $
            [ "--ghc-options=-j" <> T.pack (show w)
            , "--max-backjumps=0"
            , renderPackageId pid ] <>
            flavourArgs flavour <>
            constraintArgs (constraintsOfPackage p)

          when (flavour == Vanilla) $
            case ptype of
              -- only library packages can be described
              Just Library -> do
                Out out <- sbcabal "sandbox" ["hc-pkg", "--", "describe", renderPackageId pid]
                writeUtf8 (packageConfig penv p) out
              -- for executable only packages we just leave an empty marker
              Just ExecutablesOnly ->
                writeUtf8 (packageConfig penv p) T.empty
              -- this can't really happen, we're only supposed to assign
              -- 'ptype' to Nothing if we're not installing the vanilla flavour
              Nothing ->
                left (InstallUnknownPackageType pid)

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
createPackageSandbox :: PackageEnv -> Package -> EitherT InstallError IO PackageType
createPackageSandbox penv p@(Package (PackageRef pid _ msrc) deps _) = do
  liftIO . T.hPutStrLn stderr $ "Creating sandbox for " <> renderHashId p

  let sbdir = packageSandboxDir penv p
      sbcfg = packageSandboxConfig penv p
      sbsrc = packageSourceDir penv p

  let sbcabal x xs =
        firstT InstallCabalError $ cabalFrom sbdir (Just sbcfg) x xs

  ignoreIO (removeDirectoryRecursive sbdir)
  createDirectoryIfMissing True sbdir

  Hush <- sbcabal "sandbox" ["init", "--sandbox", sbdir]

  srcdir <- case msrc of
    -- hackage package
    Nothing -> do
      -- We install hackage packages by unpacking them in to a src/ directory
      -- inside the package's location in the global cache. This allows us to
      -- cheaply upgrade non-profiling builds to profiling builds as the .o
      -- files are kept around in the dist/ directory. It also has the benefit
      -- of not polluting the $TMPDIR on the build bot.
      createDirectoryIfMissing True sbsrc

      let
        srcdir = sbsrc </> renderPackageId pid

      Hush <- sbcabal "unpack" ["--destdir=" <> sbsrc, renderPackageId pid]
      Hush <- sbcabal "sandbox" ["add-source", srcdir]

      -- We need to shuffle anything which was unpacked to 'dist' in to
      -- 'dist-sandbox-XXX' in order to be able to install packages like
      -- 'happy' and 'alex' which have pre-baked files from their dist
      -- directory in the release tarball.
      --
      --     https://github.com/haskell/cabal/issues/2462
      --     https://github.com/commercialhaskell/stack/issues/157
      --
      let
        dist = srcdir </> "dist"
        distTmp = srcdir </> "dist-tmp"
        distSandbox = dist </> "dist-sandbox-" <> jenkins sbdir

      whenM (doesDirectoryExist dist) $ do
        renameDirectory dist distTmp
        createDirectoryIfMissing False dist
        renameDirectory distTmp distSandbox

      return srcdir

    -- source package
    Just src -> do
      Hush <- sbcabal "sandbox" ["add-source", spDirectory src]
      return (spDirectory src)

  ty <- firstT InstallCabalError $ getPackageType srcdir
  db <- firstT InstallCabalError $ readPackageDB sbcfg

  -- create symlinks to the relevant package .conf files in the
  -- package-db, then call recache so that ghc is aware of them.
  mapM_ (link db penv) (transitiveOfPackages deps)
  Hush <- sbcabal "sandbox" ["hc-pkg", "recache"]

  return ty

link :: Directory -> PackageEnv -> Package -> EitherT InstallError IO ()
link db env p@(Package (PackageRef pid _ _) _ _) = do
  let pcfg = packageConfig env p
  unlessM (doesFileExist pcfg) $
    left (InstallLinkError p pcfg)

  let dest = db </> renderPackageId pid <> ".conf"
  liftIO $ createSymbolicLink (T.unpack pcfg) (T.unpack dest)

squashRunError :: Set Package -> RunError InstallError -> InstallError
squashRunError pkgs = \case
  WorkerError (InstallPackageError pid old e) | Set.null old ->
    InstallPackageError pid pkgs e
  WorkerError e ->
    e
  BlowUpError e ->
    InstallDisaster e

------------------------------------------------------------------------

constraintsOfPackage :: Package -> [Constraint]
constraintsOfPackage p =
  let xs = Set.toList (transitiveOfPackages (Set.singleton p))
  in concatMap (packageRefConstraints . pkgRef) xs

transitiveOfPackages :: Set Package -> Set Package
transitiveOfPackages deps =
  Set.unions (deps : parMap rpar (transitiveOfPackages . pkgDeps) (Set.toList deps))

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
      T.hPutStrLn stderr ("Waiting for " <> renderHashId p <> renderFlavourSuffix f)
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

------------------------------------------------------------------------
--
-- This hash function is originally from Cabal:
--
--   https://github.com/haskell/cabal/blob/bb2e99e23b67930e081edb3d8aa7179b1fb26d29/cabal-install/Distribution/Client/Sandbox.hs#L157
--
-- See http://en.wikipedia.org/wiki/Jenkins_hash_function
--
jenkins :: Directory -> Text
jenkins =
  let
    loop :: Word32 -> Char -> Word32
    loop hash0 key_i' =
      let
        key_i = fromIntegral . ord $ key_i'
        hash1 = hash0 + key_i
        hash2 = hash1 + (shiftL hash1 10)
        hash3 = hash2 `xor` (shiftR hash2 6)
      in
        hash3

    loop_finish :: Word32 -> Word32
    loop_finish hash0 =
      let
        hash1 = hash0 + (shiftL hash0 3)
        hash2 = hash1 `xor` (shiftR hash1 11)
        hash3 = hash2 + (shiftL hash2 15)
      in
        hash3
  in
    T.pack .
    flip showHex "" .
    loop_finish .
    foldl' loop 0 .
    T.unpack
