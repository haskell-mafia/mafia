{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
module Mafia.Install
  ( Flavour(..)
  , installDependencies
  , installPackage
  , transitiveOfPackages

  , InstallError(..)
  , renderInstallError
  ) where

import           Control.Exception (SomeException)
import           Control.Monad.Trans.Bifunctor (firstT)
import           Control.Monad.Trans.Either (EitherT, pattern EitherT, left, runEitherT)
import           Control.Parallel.Strategies (rpar, parMap)
import qualified Control.Retry as Retry

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
import           Mafia.Cache
import           Mafia.IO
import           Mafia.P
import           Mafia.Package
import           Mafia.Path
import           Mafia.Process
import           Mafia.Tree
import           Mafia.Twine

import           Numeric (showHex)

import           System.IO (IO, stderr)
import qualified System.Info as Info

------------------------------------------------------------------------

data InstallError =
    InstallCacheError CacheError
  | InstallCabalError CabalError
  | InstallPackageError PackageId (Set Package) CabalError
  | InstallLinkError Package File
  | InstallEnvParseError EnvKey EnvValue Text
  | InstallPlanPackageMissing PackageName
  | InstallPlanPackageDuplicate PackageName [Package]
  | InstallUnknownPackageType PackageId
  | InstallUnpackFailed PackageId Directory (OutErrCode Text)
  | InstallDisaster SomeException
    deriving (Show)

renderInstallError :: InstallError -> Text
renderInstallError = \case
  InstallCacheError e ->
    renderCacheError e

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

  InstallUnpackFailed pid tmp out ->
    "Failed to unpack " <> renderPackageId pid <> " to " <> tmp <> "\n" <>
    renderOutErrCode out

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
  env <- firstT InstallCacheError getCacheEnv
  mapM_ (link packageDB env) tdeps
  Hush <- firstT InstallCabalError $ cabal "sandbox" ["hc-pkg", "recache"]

  return tdeps

installPackage :: PackageName -> [Constraint] -> EitherT InstallError IO Package
installPackage name constraints = do
  pkg <- firstT InstallCabalError $ findDependenciesForPackage name constraints
  installGlobalPackages Vanilla (transitiveOfPackages (Set.singleton pkg))
  return pkg

installGlobalPackages :: Flavour -> Set Package -> EitherT InstallError IO ()
installGlobalPackages flavour deps = do
  let producer q = mapM_ (writeQueue q) deps

  mw  <- getMafiaWorkers
  gw  <- getGhcWorkers
  env <- firstT InstallCacheError getCacheEnv
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
install :: NumWorkers -> CacheEnv -> Flavour -> Package -> EitherT InstallError IO ()
install w env flavour p@(Package (PackageRef pid _ _) deps _) = do
  -- only try to install this package/flavour if we haven't done it already.
  -- it's important to do this before we do the same for any dependencies
  -- otherwise we end up doing an exponential number of checks.
  let fmark = packageFlavourMarker env (pkgKey p) flavour
  unlessM (doesFileExist fmark) $ do

    -- install package dependencies
    mapM_ (install w env flavour) (transitiveOfPackages deps)

    -- detect and install build tools
    tools <- detectBuildTools p

    for_ tools $ \tool ->
      liftIO . T.hPutStrLn stderr $
        "Detected '" <> unPackageName (toolName tool) <> "' " <>
        "was required to build " <> renderPackageId pid

    paths <- installBuildTools env tools

    -- the vanilla flavour must always be installed first:
    --  + it creates and sets up the package's sandbox
    --  + profiling builds need the vanilla build for template haskell to run
    --  + documentation builds need the vanilla build to harvest the .hi files
    when (flavour /= Vanilla) $
      install w env Vanilla p

    -- we take this lock *after* all the package dependencies have been
    -- installed, otherwise we can prevent some parallelism from occurring
    unlessM (doesFileExist fmark) $
      withPackageLock env (pkgKey p) flavour $
        unlessM (doesFileExist fmark) $ do
          -- when we create the package sandbox it determines whether the
          -- package contains only executables, or is also available as a
          -- library, this is the package type.
          ptype <-
            if (flavour == Vanilla) then
              Just <$> createPackageSandbox env p
            else
              pure Nothing

          let sbdir = packageSandboxDir env (pkgKey p)
              sbcfg = packageSandboxConfig env (pkgKey p)

          let sbcabal x xs =
                firstT (InstallPackageError pid Set.empty) $ cabalFrom sbdir sbcfg paths x xs

          liftIO . T.hPutStrLn stderr $ "Building " <> renderHashId p <> renderFlavourSuffix flavour

          let platformargs =
                case Info.os of
                  "darwin" -> [
                      "--ghc-options=-optl-Wl,-dead_strip_dylibs"
                    , "--ghc-options=-optc-Wno-unused-command-line-argument"
                    , "--ghc-options=-optl-Wno-unused-command-line-argument"
                    ]
                  _ -> [
                    ]

          PassErr <- sbcabal "install" $
            platformargs <>
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
                writeUtf8 (packageConfig env $ pkgKey p) out
              -- for executable only packages we just leave an empty marker
              Just ExecutablesOnly ->
                writeUtf8 (packageConfig env $ pkgKey p) T.empty
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
createPackageSandbox :: CacheEnv -> Package -> EitherT InstallError IO PackageType
createPackageSandbox env p@(Package (PackageRef pid _ msrc) deps _) = do
  liftIO . T.hPutStrLn stderr $ "Creating sandbox for " <> renderHashId p

  let sbdir = packageSandboxDir env $ pkgKey p
      sbcfg = packageSandboxConfig env $ pkgKey p
      sbsrc = packageSourceDir env $ pkgKey p

  let sbcabal x xs =
        firstT InstallCabalError $ cabalFrom sbdir sbcfg [] x xs

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

      retryOnLeft (unpackRetryMessage pid) . capture (InstallUnpackFailed pid sbsrc) $
        sbcabal "unpack" ["--destdir=" <> sbsrc, renderPackageId pid]

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
  mapM_ (link db env) (transitiveOfPackages deps)
  Hush <- sbcabal "sandbox" ["hc-pkg", "recache"]

  return ty

-- | Install the specified build tools and return the paths to the 'bin' directories.
installBuildTools :: CacheEnv -> Set BuildTool -> EitherT InstallError IO [Directory]
installBuildTools env tools = do
  pkgs <-
    for (Set.toList tools) $ \(BuildTool name constraints) ->
      tryInstall name constraints

  pure .
    fmap (</> "bin") .
    fmap (packageSandboxDir env) $
    fmap pkgKey $
    catMaybes pkgs
 where
  -- Some packages refer to build-tools that are not cabal packages.
  -- For example, "zip-archive" depends on "unzip", but this is talking about a binary, not a cabal package.
  -- This is unfortunate, and perhaps the package shouldn't include this as it isn't a build tool, but we should probably provide some way to continue building regardless.
  tryInstall name constraints = EitherT $ do
    r <- runEitherT $ installPackage name constraints
    case r of
     Left e -> do
      T.hPutStrLn stderr $
        "Error while trying to install build tool '" <> unPackageName name <> "': "
      T.hPutStrLn stderr $ renderInstallError e
      T.hPutStrLn stderr "Trying to continue anyway, as some packages refer to non-existent build tools."
      return $ Right Nothing
     Right pkg -> do
      return $ Right $ Just pkg

-- | Detect the build tools required by a package.
detectBuildTools :: Package -> EitherT InstallError IO (Set BuildTool)
detectBuildTools (Package (PackageRef pid _ msrc) _ _) =
  case msrc of
    -- hackage package
    Nothing ->
      withSystemTempDirectory "mafia-detect-build-tools-" $ \tmp -> do
        retryOnLeft (unpackRetryMessage pid) . capture (InstallUnpackFailed pid tmp) . firstT InstallCabalError $
          cabal "unpack" ["--destdir=" <> tmp, renderPackageId pid]
        firstT InstallCabalError .
          getBuildTools $ tmp </> renderPackageId pid

    -- source package
    Just src ->
      firstT InstallCabalError .
        getBuildTools $ spDirectory src

unpackRetryMessage :: PackageId -> Text
unpackRetryMessage pid =
  "Retrying download of " <> renderPackageId pid <> "..."

retryOnLeft :: Text -> EitherT InstallError IO a -> EitherT InstallError IO a
retryOnLeft msg io =
  let
    retries =
      5

    policy =
      Retry.exponentialBackoff 500000 {- 0.5s -} <>
      Retry.limitRetries retries

    check status = \case
      Left e -> do
        when (Retry.rsIterNumber status <= retries) $
          T.hPutStrLn stderr $ renderInstallError e
        pure True

      Right _ ->
        pure False
  in
    EitherT . Retry.retrying policy check $ \status -> do
      when (Retry.rsIterNumber status > 0) $
        T.hPutStrLn stderr msg
      runEitherT io

link :: Directory -> CacheEnv -> Package -> EitherT InstallError IO ()
link db env p@(Package (PackageRef pid _ _) _ _) = do
  let pcfg = packageConfig env $ pkgKey p
  unlessM (doesFileExist pcfg) $
    left (InstallLinkError p pcfg)

  let dest = db </> renderPackageId pid <> ".conf"
  createSymbolicLink pcfg dest

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
