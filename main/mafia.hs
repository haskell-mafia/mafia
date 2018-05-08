{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           BuildInfo_mafia
import           DependencyInfo_mafia

import           Control.Concurrent (setNumCapabilities)

import           Data.ByteString (ByteString)
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL
import           Data.Time (getCurrentTime, diffUTCTime)

import           GHC.Conc (getNumProcessors)

import           Mafia.Bin
import           Mafia.Cabal
import           Mafia.Cache
import           Mafia.Error
import           Mafia.Ghc
import           Mafia.Hoogle
import           Mafia.IO
import           Mafia.Include
import           Mafia.Init
import           Mafia.Install
import           Mafia.Lock
import           Mafia.Makefile
import           Mafia.Package
import           Mafia.Path
import           Mafia.Process
import           Mafia.Script
import           Mafia.Submodule
import           Mafia.Tree

import           Mafia.P

import           System.Environment (getArgs)
import           System.IO (BufferMode(..), hSetBuffering)
import           System.IO (IO, FilePath, stdout, stderr)

import           Control.Monad.Trans.Either (EitherT, hoistEither, left)
import           Control.Monad.Trans.Bifunctor (firstT, bimapT)

import           Mafia.Options.Applicative ( Parser, CommandFields, Mod, ReadM
                                           , action, argument, strArgument
                                           , metavar, help, long, short
                                           , option, strOption, flag, flag', str
                                           , eitherTextReader, eitherReader
                                           , cli, subparser, command', orDie)

------------------------------------------------------------------------

main :: IO ()
main = do
  nprocs <- getNumProcessors
  setNumCapabilities nprocs
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  args0 <- getArgs
  case args0 of
    path : args | isScriptPath path ->
      -- bypass optparse until https://github.com/pcapriotti/optparse-applicative/pull/234 is merged
      runOrDie $ MafiaScript (T.pack path) (fmap T.pack args)
    _ ->
      cli "mafia" buildInfoVersion dependencyInfo parser runOrDie

runOrDie :: MafiaCommand -> IO ()
runOrDie =
  orDie renderMafiaError . run

------------------------------------------------------------------------

data MafiaCommand =
    MafiaUpdate
  | MafiaHash
  | MafiaDepends DependsUI (Maybe PackageName) [Flag]
  | MafiaClean
  | MafiaBuild Profiling Warnings CoreDump AsmDump [Flag] [Argument]
  | MafiaTest [Flag] [Argument]
  | MafiaTestCI [Flag] [Argument]
  | MafiaRepl [Flag] [Argument]
  | MafiaBench [Flag] [Argument]
  | MafiaLock [Flag]
  | MafiaUnlock
  | MafiaQuick [Flag] [GhciInclude] [File]
  | MafiaWatch [Flag] [GhciInclude] File [Argument]
  | MafiaHoogle [Argument]
  | MafiaInstall [Constraint] InstallPackage
  | MafiaExport [Flag] Directory
  | MafiaImport Directory
  | MafiaScript Path [Argument]
  | MafiaExec [Argument]
  | MafiaCFlags
    deriving (Eq, Show)

data Warnings =
    DisableWarnings
  | EnableWarnings
    deriving (Eq, Show)

data CoreDump =
    DisableCoreDump
  | EnableCoreDump
    deriving (Eq, Show)

data AsmDump =
    DisableAsmDump
  | EnableAsmDump
    deriving (Eq, Show)

data GhciInclude =
    Directory Directory
  | ProjectLibraries
  | AllLibraries
    deriving (Eq, Show)

data DependsUI =
    List
  | Tree
    deriving (Eq, Show)

run :: MafiaCommand -> EitherT MafiaError IO ()
run = \case
  MafiaUpdate ->
    mafiaUpdate
  MafiaHash ->
    mafiaHash
  MafiaDepends tree pkg flags ->
    mafiaDepends tree pkg flags
  MafiaClean ->
    mafiaClean
  MafiaBuild p w dumpc dumpa flags args ->
    mafiaBuild p w dumpc dumpa flags args
  MafiaTest flags args ->
    mafiaTest flags args
  MafiaTestCI flags args ->
    mafiaTestCI flags args
  MafiaRepl flags args ->
    mafiaRepl flags args
  MafiaBench flags args ->
    mafiaBench flags args
  MafiaLock flags ->
    mafiaLock flags
  MafiaUnlock ->
    mafiaUnlock
  MafiaQuick flags incs entries ->
    mafiaQuick flags incs entries
  MafiaWatch flags incs entry args ->
    mafiaWatch flags incs entry args
  MafiaHoogle args -> do
    mafiaHoogle args
  MafiaInstall constraints ipkg ->
    mafiaInstall ipkg constraints
  MafiaExport flags dir ->
    mafiaExport flags dir
  MafiaImport dir ->
    mafiaImport dir
  MafiaScript path args ->
    mafiaScript path args
  MafiaExec args ->
    mafiaExec args
  MafiaCFlags ->
    mafiaCFlags

parser :: Parser MafiaCommand
parser =
  subparser (mconcat commands) <|> pScript

-- We only need this so that optparse generates nice help text. Having said
-- that, we will switch over to this code path instead of the getArgs hack
-- above once https://github.com/pcapriotti/optparse-applicative/pull/234 is
-- merged.
pScript :: Parser MafiaCommand
pScript =
  MafiaScript <$> pScriptPath <*> many pScriptArgs

commands :: [Mod CommandFields MafiaCommand]
commands =
 [ command' "update" "Cabal update, but limited to retrieving at most once per day."
            (pure MafiaUpdate)

 , command' "hash" ( "Hash the contents of this package. Useful for checking if a "
                  <> ".mafiaignore file is working correctly. The hash denoted "
                  <> "by (package) in this command's output is the one used by "
                  <> "mafia to track changes to source dependencies." )
            (pure MafiaHash)

 , command' "depends" "Show the transitive dependencies of the this package."
            (MafiaDepends <$> pDependsUI <*> optional pDependsPackageName <*> many pFlag)

 , command' "clean" "Clean up after build. Removes the sandbox and the dist directory."
            (pure MafiaClean)

 , command' "build" "Build this package, including all executables and test suites."
            (MafiaBuild <$> pProfiling <*> pWarnings <*> pCoreDump <*> pAsmDump <*> many pFlag <*> many pCabalArgs)

 , command' "test" "Test this package, by default this runs all test suites."
            (MafiaTest <$> many pFlag <*> many pCabalArgs)

 , command' "testci" ("Test this package, but process control characters (\\b, \\r) which "
                   <> "reposition the cursor, prior to emitting each line of output.")
            (MafiaTestCI <$> many pFlag <*> many pCabalArgs)

 , command' "repl" "Start the repl, by default on the main library source."
            (MafiaRepl <$> many pFlag <*> many pCabalArgs)

 , command' "bench" "Run package benchmarks"
            (MafiaBench <$> many pFlag <*> many pCabalArgs)

 , command' "lock" "Lock down the versions of all this packages transitive dependencies."
            (MafiaLock <$> many pFlag)

 , command' "unlock" "Allow the cabal solver to choose new versions of packages again."
            (pure MafiaUnlock)

 , command' "quick" ( ghciText <> " This is an alias for the \"ghci\" command." )
            (MafiaQuick <$> many pFlag <*> pGhciIncludes <*> many pGhciEntryPoint)

 , command' "ghci" ghciText
            (MafiaQuick <$> many pFlag <*> pGhciIncludes <*> many pGhciEntryPoint)

 , command' "watch" ( "Watches filesystem for changes and stays running, compiles "
                   <> "and gives quick feedback. "
                   <> "Similarly to quick needs an entrypoint. "
                   <> "To run tests use '-T EXPR' i.e. "
                   <> "mafia watch test/test.hs -- -T Test.Pure.tests" )
            (MafiaWatch <$> many pFlag <*> pGhciIncludes <*> pGhciEntryPoint <*> many pGhcidArgs)

 , command' "hoogle" ( "Run a hoogle query across the local dependencies" )
            (MafiaHoogle <$> many pCabalArgs)

 , command' "install" ( "Install a hackage package and print the path to its bin directory. "
                     <> "The general usage is as follows:  $(mafia install pretty-show)/ppsh" )
            (MafiaInstall <$> many pConstraint <*> pInstallPackage)

 , command' "export" "Export binary substitutes of the current package's dependencies."
            (MafiaExport <$> many pFlag <*> pExportDirectory)

 , command' "import" "Import binary substitutes from a directory."
            (MafiaImport <$> pImportDirectory)

 , command' "exec" "Exec the provided command line in the local cabal sandbox."
            (MafiaExec <$> many pCabalArgs)

 , command' "cflags" "Print the flags required to compile C sources"
            (pure MafiaCFlags)

 ]
  where
    ghciText = "Start the repl directly skipping cabal, this is useful "
                <> "developing across multiple source trees at once or loading "
                <> "a not-yet-compiling package."

pExportDirectory :: Parser Directory
pExportDirectory =
  strArgument $
       metavar "EXPORT_DIRECTORY"
    <> help "The location to write binary substitute tarballs containing the current package's dependencies."

pImportDirectory :: Parser Directory
pImportDirectory =
  strArgument $
       metavar "IMPORT_DIRECTORY"
    <> help "The location to import binary substitute tarballs from."

pProfiling :: Parser Profiling
pProfiling =
  flag DisableProfiling EnableProfiling $
       long "profiling"
    <> short 'p'
    <> help "Enable profiling for this build."

pWarnings :: Parser Warnings
pWarnings =
  flag EnableWarnings DisableWarnings $
       long "disable-warnings"
    <> short 'w'
    <> help "Disable warnings for this build."

pCoreDump :: Parser CoreDump
pCoreDump =
  flag DisableCoreDump EnableCoreDump $
       long "dump-core"
    <> help "Dump the optimised Core output to dist/build/*. This is simply a shorthand for other GHC options."

pAsmDump :: Parser AsmDump
pAsmDump =
  flag DisableAsmDump EnableAsmDump $
       long "dump-asm"
    <> help "Dump the assembly-language output to dist/build/*. This is simply a shorthand for other GHC options."

pDependsUI :: Parser DependsUI
pDependsUI =
  flag List Tree $
       long "tree"
    <> short 't'
    <> help "Display dependencies as a tree."

pInstallPackage :: Parser InstallPackage
pInstallPackage =
  let
    parse txt =
      fromMaybe
        (InstallPackageName $ mkPackageName txt)
        (fmap InstallPackageId $ parsePackageId txt)
  in
    fmap parse . strArgument $
         metavar "PACKAGE"
      <> help "Install this <package> or (<package>-<version>) from Hackage."

pDependsPackageName :: Parser PackageName
pDependsPackageName =
  fmap mkPackageName . strArgument $
       metavar "PACKAGE"
    <> help "Only include packages in the output which depend on this package."

pGhciEntryPoint :: Parser File
pGhciEntryPoint =
  strArgument $
       metavar "FILE"
    <> action "file"
    <> help "The entry point for GHCi."

pGhciIncludes :: Parser [GhciInclude]
pGhciIncludes =
  fmap concat $ sequenceA [
      many pGhciIncludeDirectory
    , toList <$> optional pGhciIncludeProjectLibraries
    , toList <$> optional pGhciIncludeAllLibraries
    ]

pGhciIncludeProjectLibraries :: Parser GhciInclude
pGhciIncludeProjectLibraries =
  flag' ProjectLibraries $
       long "project"
    <> short 'p'
    <> help "Make all project source directories available for GHCi, does not include submodules."

pGhciIncludeAllLibraries :: Parser GhciInclude
pGhciIncludeAllLibraries =
  flag' AllLibraries $
       long "all"
    <> short 'a'
    <> help "Make all source directories available for GHCi, even from submodules."

pGhciIncludeDirectory :: Parser GhciInclude
pGhciIncludeDirectory =
  fmap Directory . strOption $
       long "include"
    <> short 'i'
    <> metavar "DIRECTORY"
    <> help "An additional source directory for GHCi."

pFlag :: Parser Flag
pFlag =
  option (parseFlag =<< str) $
       long "flag"
    <> short 'f'
    <> metavar "FLAG"
    <> help "Flag to pass to cabal configure."

pCabalArgs :: Parser Argument
pCabalArgs =
  strArgument $
       metavar "CABAL_ARGUMENTS"
    <> help "Extra arguments to pass on to cabal."

pGhcidArgs :: Parser Argument
pGhcidArgs =
  strArgument $
       metavar "GHCID_ARGUMENTS"
    <> help "Extra arguments to pass on to ghcid."

pConstraint :: Parser Constraint
pConstraint =
 option (eitherTextReader renderCabalError parseConstraint) $
       long "constraint"
    <> help "Specify constraints on a package (version, installed/source, flags)"

pScriptPath :: Parser File
pScriptPath =
  argument scriptRead $
       metavar "SCRIPT_PATH"
    <> help "The path to a Haskell script to execute."

pScriptArgs :: Parser File
pScriptArgs =
  strArgument $
       metavar "SCRIPT_ARGUMENTS"
    <> help "Arguments to pass to the script."

scriptRead :: ReadM File
scriptRead =
  eitherReader $ \path ->
    if isScriptPath path then
      Left $
        "Something went wrong, '" <> path <> "' looks like a script, but script" <>
        " execution should have been handled in an earlier code path.\n" <>
        "Please report this as a bug: https://github.com/haskell-mafia/mafia/issues"
    else
      Left $
        "Invalid argument '" <> path <> "', not a valid command or a valid script path.\n" <>
        "Note: paths to scripts must contain a slash, e.g. ./" <> path <> " or /usr/bin/" <> path

-- | Detect if an argument is a path to a script.
--
--   The most robust way I can think of is to look for a slash:
--
--   - Sub-commands will never contain a slash.
--
--   - If the script is on the PATH we'll receive the absolute path which will
--     contain a slash.
--
--   - If it's not on the PATH then the user will need to include a slash in
--     the relative path in order to execute it anyway, e.g. ./script or dir/script
--
isScriptPath :: FilePath -> Bool
isScriptPath =
  List.isInfixOf "/"

------------------------------------------------------------------------

mafiaUpdate :: EitherT MafiaError IO ()
mafiaUpdate = do
  home <- getHomeDirectory

  let index = home </> ".cabal/packages/hackage.haskell.org/00-index.cache"

  mindexTime <- getModificationTime index

  case mindexTime of
    Nothing ->
      liftCabal $ cabal_ "update" []

    Just indexTime -> do
      currentTime <- liftIO getCurrentTime

      let
        age = currentTime `diffUTCTime` indexTime
        oneDay = 24 * 60 * 60

      when (age > oneDay) $
        liftCabal $ cabal_ "update" []

mafiaHash :: EitherT MafiaError IO ()
mafiaHash = do
  sph <- liftCabal (hashSourcePackage ".")
  liftIO (T.putStr (renderSourcePackageHash sph))

mafiaDepends :: DependsUI -> Maybe PackageName -> [Flag] -> EitherT MafiaError IO ()
mafiaDepends ui mpkg flags = do
  lockFile <- firstT MafiaLockError $ getLockFile =<< getCurrentDirectory
  constraints <- fmap (fromMaybe []) . firstT MafiaLockError $ readLockFile lockFile
  sdeps <- fmap Set.toList . firstT MafiaInitError $ getSourceDependencies
  local <- firstT MafiaCabalError (findDependenciesForCurrentDirectory flags sdeps constraints)
  let
    deps = maybe id filterPackages mpkg $ pkgDeps local
  case ui of
    List -> do
      let trans = Set.toList $ transitiveOfPackages deps
      traverse_ (liftIO . T.putStrLn . renderPackageRef . pkgRef) trans
    Tree ->
      liftIO . TL.putStr $ renderTree deps

mafiaClean :: EitherT MafiaError IO ()
mafiaClean = do
  -- "Out _" ignores the spurious "cleaning..." message that cabal emits on success
  Out (_ :: ByteString) <- liftCabal $ cabal "clean" []
  liftCabal removeSandbox

mafiaBuild :: Profiling -> Warnings -> CoreDump -> AsmDump -> [Flag] -> [Argument] -> EitherT MafiaError IO ()
mafiaBuild p w dumpc dumpa flags args = do
  initMafia LatestSources p flags

  let
    wargs =
      case w of
        DisableWarnings ->
          ["--ghc-options=-w"]
        EnableWarnings ->
          ["--ghc-options=-Werror"]

    dumpargs =
      ordNub $ coredump <> asmdump

    coredump =
      case dumpc of
        DisableCoreDump ->
          []
        EnableCoreDump -> fmap ("--ghc-options="<>)
          ["-ddump-simpl"
          ,"-ddump-to-file"
          ,"-dppr-case-as-let"
          ,"-dsuppress-uniques"
          ,"-dsuppress-idinfo"
          ,"-dsuppress-coercions"
          ,"-dsuppress-type-applications"
          ,"-dsuppress-module-prefixes"
          ]

    asmdump =
      case dumpa of
        DisableAsmDump ->
          []
        EnableAsmDump -> fmap ("--ghc-options="<>)
          ["-ddump-asm"
          ,"-ddump-to-file"
          ]

  liftCabal . cabal_ "build" $ ["-j"] <> wargs <> dumpargs <> args

mafiaTest :: [Flag] -> [Argument] -> EitherT MafiaError IO ()
mafiaTest flags args = do
  initMafia LatestSources DisableProfiling flags
  liftCabal . cabalAnnihilate "test" $ ["-j", "--show-details=streaming"] <> args

mafiaTestCI :: [Flag] -> [Argument] -> EitherT MafiaError IO ()
mafiaTestCI flags args = do
  initMafia LatestSources DisableProfiling flags
  Clean <- liftCabal . cabal "test" $ ["-j", "--show-details=streaming"] <> args
  return ()

mafiaRepl :: [Flag] -> [Argument] -> EitherT MafiaError IO ()
mafiaRepl flags args = do
  initMafia LatestSources DisableProfiling flags
  liftCabal $ cabal_ "repl" args

mafiaBench :: [Flag] -> [Argument] -> EitherT MafiaError IO ()
mafiaBench flags args = do
  initMafia LatestSources DisableProfiling flags
  liftCabal $ cabalAnnihilate "bench" args

mafiaLock :: [Flag] -> EitherT MafiaError IO ()
mafiaLock flags = do
  initMafia LatestSources DisableProfiling flags
  mconstraints <- firstT MafiaInitError readInstallConstraints
  case mconstraints of
    Nothing ->
      left MafiaNoInstallConstraints
    Just constraints -> do
      lockFile <- firstT MafiaLockError $ getLockFile =<< getCurrentDirectory
      firstT MafiaLockError $ writeLockFile lockFile constraints

mafiaUnlock :: EitherT MafiaError IO ()
mafiaUnlock = do
  lockFile <- firstT MafiaLockError $ getLockFile =<< getCurrentDirectory
  ignoreIO $ removeFile lockFile

mafiaQuick :: [Flag] -> [GhciInclude] -> [File] -> EitherT MafiaError IO ()
mafiaQuick flags extraIncludes paths = do
  dir <- getWorkingDirectory $ listToMaybe paths
  args <- ghciArgs dir extraIncludes paths
  withDirectory dir $ initMafia (vintageOfIncludes extraIncludes) DisableProfiling flags
  exec MafiaProcessError "ghci" args

mafiaWatch :: [Flag] -> [GhciInclude] -> File -> [Argument] -> EitherT MafiaError IO ()
mafiaWatch flags extraIncludes path extraArgs = do
  ghcidExe <- bimapT MafiaBinError (</> "ghcid") $ installBinary (ipackageId "ghcid" [0, 6, 7]) []
  dir <- getWorkingDirectory $ Just path
  args <- ghciArgs dir extraIncludes [path]
  withDirectory dir $ initMafia (vintageOfIncludes extraIncludes) DisableProfiling flags
  exec MafiaProcessError ghcidExe $ [ "-c", T.unwords ("ghci" : args) ] <> extraArgs

mafiaHoogle :: [Argument] -> EitherT MafiaError IO ()
mafiaHoogle args = do
  hkg <- fromMaybe "https://hackage.haskell.org/package" <$> lookupEnv "HACKAGE"
  firstT MafiaInitError (initialize LatestSources Nothing Nothing)
  hoogle hkg args

mafiaInstall :: InstallPackage -> [Constraint] -> EitherT MafiaError IO ()
mafiaInstall ipkg constraints = do
  liftIO . T.putStrLn =<< firstT MafiaBinError (installBinary ipkg constraints)

mafiaExport :: [Flag] -> Directory -> EitherT MafiaError IO ()
mafiaExport flags dir = do
  initMafia LatestSources DisableProfiling flags
  mkeys <- firstT MafiaInitError readPackageKeys
  case mkeys of
    Nothing ->
      left MafiaNoPackageKeys
    Just keys -> do
      env <- firstT MafiaCacheError getCacheEnv
      for_ keys $ \key -> do
        result <- firstT MafiaCacheError $ exportPackage env key dir
        liftIO . T.putStrLn $ renderExportResult result

mafiaImport :: Directory -> EitherT MafiaError IO ()
mafiaImport dir = do
  env <- firstT MafiaCacheError getCacheEnv
  keys <- firstT MafiaCacheError $ listPackages env dir
  for_ keys $ \key -> do
    result <- firstT MafiaCacheError $ importPackage env key dir
    liftIO . T.putStrLn $ renderImportResult result

mafiaScript :: File -> [Argument] -> EitherT MafiaError IO ()
mafiaScript file args =
  firstT MafiaScriptError $ runScript file args

mafiaExec :: [Argument] -> EitherT MafiaError IO ()
mafiaExec args = do
  let fixedArgs =
        case args of
          [] -> [] -- Should not happen.
          [x] -> [x] -- A command without arguments.
          -- Insert `--` between the command and its arguments so cabal doesn't
          -- mess with them.
          (x:xs) -> x : "--" : xs
  exec MafiaProcessError "cabal" $ "exec" : fixedArgs

mafiaCFlags :: EitherT MafiaError IO ()
mafiaCFlags = do
  dirs <- getIncludeDirs
  printIncludes dirs
 where
  printIncludes dirs = liftIO $ do
    mapM_ (\d -> T.putStr " -I" >> T.putStr d) dirs
    T.putStrLn ""

getWorkingDirectory :: Maybe File -> EitherT MafiaError IO Directory
getWorkingDirectory mpath =
  case mpath of
    Nothing ->
      getCurrentDirectory
    Just path -> do
      mdir <- findCabalRoot path
      case mdir of
        Nothing ->
          getCurrentDirectory
        Just dir ->
          return dir

withDirectory :: MonadIO m => Directory -> m a -> m a
withDirectory dir io = do
  old <- getCurrentDirectory
  setCurrentDirectory dir
  x <- io
  setCurrentDirectory old
  return x

vintageOfIncludes :: [GhciInclude] -> SourceVintage
vintageOfIncludes xs =
  if any isProjectLibraries xs then
    PermitStaleProjectSources
  else
    LatestSources

isProjectLibraries :: GhciInclude -> Bool
isProjectLibraries = \case
  ProjectLibraries ->
    True
  Directory _ ->
    False
  AllLibraries ->
    False

ghciArgs :: Directory -> [GhciInclude] -> [File] -> EitherT MafiaError IO [Argument]
ghciArgs wdir extraIncludes paths = do
  mapM_ checkEntryPoint paths

  standard <- mapM (tryMakeRelativeToCurrent . (wdir </>)) standardSourceDirs
  extras <- concat <$> mapM (reifyInclude wdir) extraIncludes

  let
    dirs =
      standard <> extras

  headers <- getHeaders
  includes <- catMaybes <$> mapM ensureDirectory dirs
  databases <- mapM tryMakeRelativeToCurrent =<< getPackageDatabases wdir

  return $ mconcat [
      [ "-no-user-package-db" ]
    , concatMap (\x -> ["-optP-include", "-optP" <> x]) headers
    , fmap ("-i" <>) includes
    , fmap ("-package-db=" <>) databases
    , paths
    ]

getPackageDatabases :: Directory -> EitherT MafiaError IO [Directory]
getPackageDatabases wdir =
  withDirectory wdir $ do
    let
      isPackage =
        ("-packages.conf.d" `T.isSuffixOf`)

    sandboxDir <- liftCabal initSandbox
    filter isPackage <$> getDirectoryListing Recursive (wdir </> sandboxDir)

getHeaders :: EitherT MafiaError IO [File]
getHeaders = do
  version <- firstT MafiaGhcError getGhcVersion

  if version >= mkGhcVersion [8,0,1] then
    return []
  else do
    let
      cabalMacros =
        "dist/build/autogen/cabal_macros.h"

    ok <- doesFileExist cabalMacros

    if ok then
      return [cabalMacros]
    else
      return []

checkEntryPoint :: File -> EitherT MafiaError IO ()
checkEntryPoint file = do
  unlessM (doesFileExist file) $
    hoistEither (Left (MafiaEntryPointNotFound file))

reifyInclude :: Directory -> GhciInclude -> EitherT MafiaError IO [Directory]
reifyInclude wdir = \case
  Directory dir ->
    return [dir]

  ProjectLibraries -> do
    dirs <- firstT MafiaGitError . withDirectory wdir $
      mapM canonicalizePath =<< fmap Set.toList getProjectSources

    concatMap appendStandardDirs <$> mapM tryMakeRelativeToCurrent dirs

  AllLibraries -> do
    dirs <- firstT MafiaSubmoduleError . withDirectory wdir $
      mapM canonicalizePath =<< fmap Set.toList getAvailableSources

    concatMap appendStandardDirs <$> mapM tryMakeRelativeToCurrent dirs

appendStandardDirs :: Directory -> [Directory]
appendStandardDirs dir =
  fmap (dir </>) standardSourceDirs

standardSourceDirs :: [Directory]
standardSourceDirs =
  ["src", "test", "gen", "dist/build", "dist/build/autogen"]

ensureDirectory :: MonadIO m => Directory -> m (Maybe Directory)
ensureDirectory dir = do
  exists <- doesDirectoryExist dir
  case exists of
    False -> return Nothing
    True  -> return (Just dir)

resolveVintage :: MonadIO m => SourceVintage -> m SourceVintage
resolveVintage x0 = do
  mx <- liftIO $ lookupEnv "MAFIA_FORCE_LATEST_SOURCES"
  case mx of
    Just "1" ->
      return LatestSources
    Just "true" ->
      return LatestSources
    Just _ ->
      return x0
    Nothing ->
      return x0

initMafia :: SourceVintage -> Profiling -> [Flag] -> EitherT MafiaError IO ()
initMafia vintage0 prof flags = do
  vintage <- resolveVintage vintage0

  -- we just call this for the side-effect, if we can't find a .cabal file then
  -- mafia should fail fast and not polute the directory with a sandbox.
  (_ :: File) <- firstT MafiaCabalError $ getCabalFile =<< getCurrentDirectory

  ensureBuildTools

  firstT MafiaInitError $ initialize vintage (Just prof) (Just flags)

  directory <- firstT MafiaCabalError $ getCurrentDirectory
  buildMakefile directory

ensureBuildTools :: EitherT MafiaError IO ()
ensureBuildTools = do
  tools <- firstT MafiaCabalError $ getBuildTools =<< getCurrentDirectory

  firstT MafiaBinError . for_ tools $ \(BuildTool name constraints) ->
    installOnPath (InstallPackageName name) constraints
