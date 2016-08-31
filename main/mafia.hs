{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           BuildInfo_ambiata_mafia
import           DependencyInfo_ambiata_mafia

import           Control.Concurrent (setNumCapabilities)
import           Control.Monad.IO.Class (MonadIO(..))

import           Data.ByteString (ByteString)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy.IO as TL
import           Data.Time (getCurrentTime, diffUTCTime)

import           GHC.Conc (getNumProcessors)

import           Mafia.Bin
import           Mafia.Cabal
import           Mafia.Error
import           Mafia.Hoogle
import           Mafia.IO
import           Mafia.Init
import           Mafia.Install
import           Mafia.Lock
import           Mafia.Package
import           Mafia.Path
import           Mafia.Process
import           Mafia.Project
import           Mafia.Submodule
import           Mafia.Tree

import           P hiding (Last)

import           System.Exit (exitSuccess)
import           System.IO (BufferMode(..), hSetBuffering)
import           System.IO (IO, stdout, stderr, putStrLn, print)

import           X.Control.Monad.Trans.Either (EitherT, hoistEither, left)
import           X.Control.Monad.Trans.Either.Exit (orDie)
import           X.Options.Applicative (Parser, CommandFields, Mod)
import           X.Options.Applicative (SafeCommand(..), RunType(..))
import           X.Options.Applicative (argument, textRead, metavar, help, long, short, option, flag, flag', eitherTextReader)
import           X.Options.Applicative (dispatch, subparser, safeCommand, command')

------------------------------------------------------------------------

main :: IO ()
main = do
  nprocs <- getNumProcessors
  setNumCapabilities nprocs
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  dispatch parser >>= \case
    VersionCommand ->
      putStrLn buildInfoVersion >> exitSuccess
    DependencyCommand ->
      traverse putStrLn dependencyInfo >> exitSuccess
    RunCommand DryRun c ->
      print c >> exitSuccess
    RunCommand RealRun c ->
      orDie renderMafiaError (run c)

------------------------------------------------------------------------

data MafiaCommand =
    MafiaUpdate
  | MafiaHash
  | MafiaDepends DependsUI (Maybe PackageName) [Flag]
  | MafiaClean
  | MafiaBuild Profiling Warnings CoreDump [Flag] [Argument]
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
    deriving (Eq, Show)

data Warnings =
    DisableWarnings
  | EnableWarnings
    deriving (Eq, Show)

data CoreDump =
    DisableCoreDump
  | EnableCoreDump
    deriving (Eq, Show)

data GhciInclude =
    Directory Directory
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
  MafiaBuild p w dump flags args ->
    mafiaBuild p w dump flags args
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
  MafiaInstall constraints ipkg -> do
    mafiaInstall ipkg constraints

parser :: Parser (SafeCommand MafiaCommand)
parser = safeCommand . subparser . mconcat $ commands

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

 , command' "build" "Build this project, including all executables and test suites."
            (MafiaBuild <$> pProfiling <*> pWarnings <*> pCoreDump <*> many pFlag <*> many pCabalArgs)

 , command' "test" "Test this project, by default this runs all test suites."
            (MafiaTest <$> many pFlag <*> many pCabalArgs)

 , command' "testci" ("Test this project, but process control characters (\\b, \\r) which "
                   <> "reposition the cursor, prior to emitting each line of output.")
            (MafiaTestCI <$> many pFlag <*> many pCabalArgs)

 , command' "repl" "Start the repl, by default on the main library source."
            (MafiaRepl <$> many pFlag <*> many pCabalArgs)

 , command' "bench" "Run project benchmarks"
            (MafiaBench <$> many pFlag <*> many pCabalArgs)

 , command' "lock" "Lock down the versions of all this packages transitive dependencies."
            (MafiaLock <$> many pFlag)

 , command' "unlock" "Allow the cabal solver to choose new versions of packages again."
            (pure MafiaUnlock)

 , command' "quick" ( "Start the repl directly skipping cabal, this is useful "
                   <> "developing across multiple source trees at once." )
            (MafiaQuick <$> many pFlag <*> pGhciIncludes <*> some pGhciEntryPoint)

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
 ]

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
    fmap parse . argument textRead $
         metavar "PACKAGE"
      <> help "Install this <package> or (<package>-<version>) from Hackage."

pDependsPackageName :: Parser PackageName
pDependsPackageName =
  fmap mkPackageName . argument textRead $
       metavar "PACKAGE"
    <> help "Only include packages in the output which depend on this package."

pGhciEntryPoint :: Parser File
pGhciEntryPoint =
  argument textRead $
       metavar "FILE"
    <> help "The entry point for GHCi."

pGhciIncludes :: Parser [GhciInclude]
pGhciIncludes = many (pGhciIncludeDirectory <|> pGhciIncludeAllLibraries)

pGhciIncludeAllLibraries :: Parser GhciInclude
pGhciIncludeAllLibraries =
  flag' AllLibraries $
       long "all"
    <> short 'a'
    <> help "Include all library source directories for GHCi."

pGhciIncludeDirectory :: Parser GhciInclude
pGhciIncludeDirectory =
  fmap Directory . option textRead $
       long "include"
    <> short 'i'
    <> metavar "DIRECTORY"
    <> help "An additional source directory for GHCi."

pFlag :: Parser Flag
pFlag =
  option (parseFlag =<< textRead) $
       long "flag"
    <> short 'f'
    <> metavar "FLAG"
    <> help "Flag to pass to cabal configure."

pCabalArgs :: Parser Argument
pCabalArgs =
  argument textRead $
       metavar "CABAL_ARGUMENTS"
    <> help "Extra arguments to pass on to cabal."

pGhcidArgs :: Parser Argument
pGhcidArgs =
  argument textRead $
       metavar "GHCID_ARGUMENTS"
    <> help "Extra arguments to pass on to ghcid."

pConstraint :: Parser Constraint
pConstraint =
 option (eitherTextReader renderCabalError parseConstraint) $
       long "constraint"
    <> help "Specify constraints on a package (version, installed/source, flags)"

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
  sdeps <- Set.toList <$> firstT MafiaInitError getSourceDependencies
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

mafiaBuild :: Profiling -> Warnings -> CoreDump -> [Flag] -> [Argument] -> EitherT MafiaError IO ()
mafiaBuild p w dump flags args = do
  initMafia p flags

  let
    wargs =
      case w of
        DisableWarnings ->
          ["--ghc-options=-w"]
        EnableWarnings ->
          ["--ghc-options=-Werror"]

    dumpargs =
      case dump of
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


  liftCabal . cabal_ "build" $ ["-j"] <> wargs <> dumpargs <> args

mafiaTest :: [Flag] -> [Argument] -> EitherT MafiaError IO ()
mafiaTest flags args = do
  initMafia DisableProfiling flags
  liftCabal . cabal_ "test" $ ["-j", "--show-details=streaming"] <> args

mafiaTestCI :: [Flag] -> [Argument] -> EitherT MafiaError IO ()
mafiaTestCI flags args = do
  initMafia DisableProfiling flags
  Clean <- liftCabal . cabal "test" $ ["-j", "--show-details=streaming"] <> args
  return ()

mafiaRepl :: [Flag] -> [Argument] -> EitherT MafiaError IO ()
mafiaRepl flags args = do
  initMafia DisableProfiling flags
  liftCabal $ cabal_ "repl" args

mafiaBench :: [Flag] -> [Argument] -> EitherT MafiaError IO ()
mafiaBench flags args = do
  initMafia DisableProfiling flags
  liftCabal $ cabal_ "bench" args

mafiaLock :: [Flag] -> EitherT MafiaError IO ()
mafiaLock flags = do
  initMafia DisableProfiling flags
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
  args <- ghciArgs extraIncludes paths
  initMafia DisableProfiling flags
  exec MafiaProcessError "ghci" args

mafiaWatch :: [Flag] -> [GhciInclude] -> File -> [Argument] -> EitherT MafiaError IO ()
mafiaWatch flags extraIncludes path extraArgs = do
  ghcidExe <- bimapT MafiaBinError (</> "ghcid") $ installBinary (ipackageId "ghcid" [0, 5]) []
  args <- ghciArgs extraIncludes [path]
  initMafia DisableProfiling flags
  exec MafiaProcessError ghcidExe $ [ "-c", T.unwords ("ghci" : args) ] <> extraArgs

mafiaHoogle :: [Argument] -> EitherT MafiaError IO ()
mafiaHoogle args = do
  hkg <- fromMaybe "https://hackage.haskell.org/package" <$> lookupEnv "HACKAGE"
  firstT MafiaInitError (initialize Nothing Nothing)
  hoogle hkg args

mafiaInstall :: InstallPackage -> [Constraint] -> EitherT MafiaError IO ()
mafiaInstall ipkg constraints = do
  liftIO . T.putStrLn =<< firstT MafiaBinError (installBinary ipkg constraints)

ghciArgs :: [GhciInclude] -> [File] -> EitherT MafiaError IO [Argument]
ghciArgs extraIncludes paths = do
  mapM_ checkEntryPoint paths

  extras <- concat <$> mapM reifyInclude extraIncludes

  let dirs = ["src", "test", "gen", "dist/build/autogen"] <> extras
  includes  <- catMaybes <$> mapM ensureDirectory dirs
  databases <- getPackageDatabases

  return $ [ "-no-user-package-db" ]
        <> (fmap ("-i" <>)           includes)
        <> (fmap ("-package-db=" <>) databases)
        <> paths

checkEntryPoint :: File -> EitherT MafiaError IO ()
checkEntryPoint file = do
  unlessM (doesFileExist file) $
    hoistEither (Left (MafiaEntryPointNotFound file))

reifyInclude :: GhciInclude -> EitherT MafiaError IO [Directory]
reifyInclude = \case
  Directory dir -> return [dir]
  AllLibraries  -> do
    absDirs <- Set.toList <$> firstT MafiaSubmoduleError getSubmoduleSources
    relDirs <- mapM tryMakeRelativeToCurrent absDirs
    return [ dir </> sub | dir <- relDirs
                         , sub <- ["src", "test", "gen", "dist/build/autogen"] ]

ensureDirectory :: MonadIO m => Directory -> m (Maybe Directory)
ensureDirectory dir = do
  exists <- doesDirectoryExist dir
  case exists of
    False -> return Nothing
    True  -> return (Just dir)

getPackageDatabases :: EitherT MafiaError IO [Directory]
getPackageDatabases = do
    sandboxDir <- liftCabal initSandbox
    filter isPackage <$> getDirectoryListing Recursive sandboxDir
  where
    isPackage = ("-packages.conf.d" `T.isSuffixOf`)

initMafia :: Profiling -> [Flag] -> EitherT MafiaError IO ()
initMafia prof flags = do
  -- we just call this for the side-effect, if we can't find a .cabal file then
  -- mafia should fail fast and not polute the directory with a sandbox.
  (_ :: ProjectName) <- firstT MafiaProjectError $ getProjectName =<< getCurrentDirectory

  ensureBuildTools

  firstT MafiaInitError $ initialize (Just prof) (Just flags)

ensureBuildTools :: EitherT MafiaError IO ()
ensureBuildTools = do
  tools <- firstT MafiaCabalError $ getBuildTools =<< getCurrentDirectory

  firstT MafiaBinError . for_ tools $ \(BuildTool name constraints) ->
    installOnPath (InstallPackageName name) constraints
