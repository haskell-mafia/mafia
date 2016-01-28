{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           BuildInfo_ambiata_mafia

import           Control.Concurrent (setNumCapabilities)
import           Control.Monad.IO.Class (MonadIO(..))

import           Data.ByteString (ByteString)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Time (getCurrentTime, diffUTCTime)

import           GHC.Conc (getNumProcessors)

import           Mafia.Cabal
import           Mafia.Error
import           Mafia.Home
import           Mafia.Hoogle
import           Mafia.IO
import           Mafia.Init
import           Mafia.Package
import           Mafia.Path
import           Mafia.Process
import           Mafia.Project
import           Mafia.Submodule

import           P

import           System.Exit (exitSuccess)
import           System.IO (BufferMode(..), hSetBuffering)
import           System.IO (IO, stdout, stderr, putStrLn, print)

import           X.Control.Monad.Trans.Either (EitherT)
import           X.Control.Monad.Trans.Either (bimapEitherT, firstEitherT, hoistEither)
import           X.Control.Monad.Trans.Either.Exit (orDie)
import           X.Options.Applicative (Parser, CommandFields, Mod)
import           X.Options.Applicative (SafeCommand(..), RunType(..))
import           X.Options.Applicative (argument, textRead, metavar, help, long, short, option, flag, flag')
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
    RunCommand DryRun c ->
      print c >> exitSuccess
    RunCommand RealRun c ->
      orDie renderMafiaError (run c)

------------------------------------------------------------------------

data MafiaCommand =
    MafiaUpdate
  | MafiaHash
  | MafiaClean
  | MafiaBuild Profiling [Argument]
  | MafiaTest [Argument]
  | MafiaTestCI [Argument]
  | MafiaRepl [Argument]
  | MafiaBench [Argument]
  | MafiaQuick [GhciInclude] [File]
  | MafiaWatch [GhciInclude] File [Argument]
  | MafiaHoogle [Argument]
    deriving (Eq, Show)

data GhciInclude =
    Directory Directory
  | AllLibraries
    deriving (Eq, Show)

run :: MafiaCommand -> EitherT MafiaError IO ()
run = \case
  MafiaUpdate ->
    mafiaUpdate
  MafiaHash ->
    mafiaHash
  MafiaClean ->
    mafiaClean
  MafiaBuild p args ->
    mafiaBuild p args
  MafiaTest args ->
    mafiaTest args
  MafiaTestCI args ->
    mafiaTestCI args
  MafiaRepl args ->
    mafiaRepl args
  MafiaBench args ->
    mafiaBench args
  MafiaQuick incs entries ->
    mafiaQuick incs entries
  MafiaWatch incs entry args ->
    mafiaWatch incs entry args
  MafiaHoogle args -> do
    mafiaHoogle args

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

 , command' "clean" "Clean up after build. Removes the sandbox and the dist directory."
            (pure MafiaClean)

 , command' "build" "Build this project, including all executables and test suites."
            (MafiaBuild <$> pProfiling <*> many pCabalArgs)

 , command' "test" "Test this project, by default this runs all test suites."
            (MafiaTest <$> many pCabalArgs)

 , command' "testci" ("Test this project, but process control characters (\\b, \\r) which "
                   <> "reposition the cursor, prior to emitting each line of output.")
            (MafiaTestCI <$> many pCabalArgs)

 , command' "repl" "Start the repl, by default on the main library source."
            (MafiaRepl <$> many pCabalArgs)

 , command' "bench" "Run project benchmarks"
            (MafiaBench <$> many pCabalArgs)

 , command' "quick" ( "Start the repl directly skipping cabal, this is useful "
                   <> "developing across multiple source trees at once." )
            (MafiaQuick <$> pGhciIncludes <*> some pGhciEntryPoint)

 , command' "watch" ( "Watches filesystem for changes and stays running, compiles "
                   <> "and gives quick feedback. "
                   <> "Similarly to quick needs an entrypoint. "
                   <> "To run tests use '-T EXPR' i.e. "
                   <> "mafia watch test/test.hs -- -T Test.Pure.tests" )
            (MafiaWatch <$> pGhciIncludes <*> pGhciEntryPoint <*> many pGhcidArgs)

 , command' "hoogle" ( "Run a hoogle query across the local dependencies" )
            (MafiaHoogle <$> many pCabalArgs)
 ]

pProfiling :: Parser Profiling
pProfiling =
  flag DisableProfiling EnableProfiling $
       long "profiling"
    <> short 'p'
    <> help "Enable profiling for this build."

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

------------------------------------------------------------------------

mafiaUpdate :: EitherT MafiaError IO ()
mafiaUpdate = do
  home <- getHomeDirectory

  let index = home </> ".cabal/packages/hackage.haskell.org/00-index.cache"

  indexTime   <- getModificationTime index
  currentTime <- liftIO getCurrentTime

  let age    = currentTime `diffUTCTime` indexTime
      oneDay = 24 * 60 * 60

  when (age > oneDay) $
    liftCabal $ cabal_ "update" []

mafiaHash :: EitherT MafiaError IO ()
mafiaHash = do
  sph <- liftCabal (hashSourcePackage ".")
  liftIO (T.putStr (renderSourcePackageHash sph))

mafiaClean :: EitherT MafiaError IO ()
mafiaClean = do
  -- "Out _" ignores the spurious "cleaning..." message that cabal emits on success
  Out (_ :: ByteString) <- liftCabal $ cabal "clean" []
  liftCabal removeSandbox

mafiaBuild :: Profiling -> [Argument] -> EitherT MafiaError IO ()
mafiaBuild p args = do
  initMafia $ Just p
  liftCabal . cabal_ "build" $ ["-j", "--ghc-option=-Werror"] <> args

mafiaTest :: [Argument] -> EitherT MafiaError IO ()
mafiaTest args = do
  initMafia $ Just DisableProfiling
  liftCabal . cabal_ "test" $ ["-j", "--show-details=streaming"] <> args

mafiaTestCI :: [Argument] -> EitherT MafiaError IO ()
mafiaTestCI args = do
  initMafia $ Just DisableProfiling
  Clean <- liftCabal . cabal "test" $ ["-j", "--show-details=streaming"] <> args
  return ()

mafiaRepl :: [Argument] -> EitherT MafiaError IO ()
mafiaRepl args = do
  initMafia $ Just DisableProfiling
  liftCabal $ cabal_ "repl" args

mafiaBench :: [Argument] -> EitherT MafiaError IO ()
mafiaBench args = do
  initMafia $ Just DisableProfiling
  liftCabal $ cabal_ "bench" args

mafiaQuick :: [GhciInclude] -> [File] -> EitherT MafiaError IO ()
mafiaQuick extraIncludes paths = do
  args <- ghciArgs extraIncludes paths
  initMafia $ Just DisableProfiling
  exec MafiaProcessError "ghci" args

mafiaWatch :: [GhciInclude] -> File -> [Argument] -> EitherT MafiaError IO ()
mafiaWatch extraIncludes path extraArgs = do
  ghcidExe <- bimapEitherT MafiaProcessError (</> "ghcid") $ installBinary (packageId "ghcid" [0, 5]) []
  args <- ghciArgs extraIncludes [path]
  initMafia $ Just DisableProfiling
  exec MafiaProcessError ghcidExe $ [ "-c", T.unwords ("ghci" : args) ] <> extraArgs

mafiaHoogle :: [Argument] -> EitherT MafiaError IO ()
mafiaHoogle args = do
  hkg <- fromMaybe "https://hackage.haskell.org/package" <$> lookupEnv "HACKAGE"
  firstEitherT MafiaInitError (initialize Nothing)
  hoogle hkg args

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

initMafia :: Maybe Profiling -> EitherT MafiaError IO ()
initMafia mproj = do
  -- we just call this for the side-effect, if we can't find a .cabal file then
  -- mafia should fail fast and not polute the directory with a sandbox.
  (_ :: ProjectName) <- firstEitherT MafiaProjectError getProjectName

  let ensureExeOnPath' e pkg =
        lookupEnv e >>= mapM_ (\b -> when (b == "true") $ ensureExeOnPath pkg)
  firstEitherT MafiaProcessError $ ensureExeOnPath' "MAFIA_HAPPY" (packageId "happy" [1, 19, 5])
  firstEitherT MafiaProcessError $ ensureExeOnPath' "MAFIA_ALEX" (packageId "alex" [3, 1, 6])
  firstEitherT MafiaInitError $ initialize mproj
