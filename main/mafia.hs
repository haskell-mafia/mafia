{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           BuildInfo_ambiata_mafia

import           Control.Monad.IO.Class (MonadIO(..))

import qualified Data.Set as Set
import qualified Data.Text as T
import           Data.Time (getCurrentTime, diffUTCTime)

import           Mafia.Cabal
import           Mafia.Error
import           Mafia.Home
import           Mafia.Hoogle
import           Mafia.IO
import           Mafia.Init
import           Mafia.Path
import           Mafia.Process
import           Mafia.Submodule

import           P

import           System.Environment (lookupEnv)
import           System.Exit (exitSuccess)
import           System.IO (BufferMode(..), hSetBuffering)
import           System.IO (IO, stdout, stderr, putStrLn, print)

import           X.Control.Monad.Trans.Either (EitherT)
import           X.Control.Monad.Trans.Either (firstEitherT, hoistEither)
import           X.Control.Monad.Trans.Either.Exit (orDie)
import           X.Options.Applicative (Parser, CommandFields, Mod)
import           X.Options.Applicative (SafeCommand(..), RunType(..))
import           X.Options.Applicative (argument, textRead, metavar, help, long, short, option, flag')
import           X.Options.Applicative (dispatch, subparser, safeCommand, command')

------------------------------------------------------------------------

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  dispatch parser >>= \sc ->
    case sc of
      VersionCommand ->
        putStrLn buildInfoVersion >> exitSuccess
      RunCommand DryRun c ->
        print c >> exitSuccess
      RunCommand RealRun c ->
        orDie renderMafiaError (run c)

------------------------------------------------------------------------

data MafiaCommand
  = MafiaUpdate
  | MafiaBuild  [Argument]
  | MafiaTest   [Argument]
  | MafiaTestCI [Argument]
  | MafiaRepl   [Argument]
  | MafiaBench  [Argument]
  | MafiaQuick  [GhciInclude] File
  | MafiaWatch  [GhciInclude] File [Argument]
  | MafiaHoogle [Argument]
  deriving (Eq, Show)

data GhciInclude
  = Directory Directory
  | AllLibraries
  deriving (Eq, Show)

run :: MafiaCommand -> EitherT MafiaError IO ()
run = \case
  MafiaUpdate                 -> update
  MafiaBuild  args            -> build  args
  MafiaTest   args            -> test   args
  MafiaTestCI args            -> testci args
  MafiaRepl   args            -> repl   args
  MafiaBench  args            -> bench  args
  MafiaQuick  incs entry      -> quick  incs entry
  MafiaWatch  incs entry args -> watch  incs entry args
  MafiaHoogle args            -> do
    hkg <- liftIO . fmap (T.pack . fromMaybe "https://hackage.haskell.org/package") $ lookupEnv "HACKAGE"
    initialize
    hoogle hkg args

parser :: Parser (SafeCommand MafiaCommand)
parser = safeCommand . subparser . mconcat $ commands

commands :: [Mod CommandFields MafiaCommand]
commands =
 [ command' "update" "Cabal update, but limited to retrieving at most once per day."
            (pure MafiaUpdate)

 , command' "build" "Build this project, including all executables and test suites."
            (MafiaBuild <$> many pCabalArgs)

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
            (MafiaQuick <$> pGhciIncludes <*> pGhciEntryPoint)

 , command' "watch" ( "Watches filesystem for changes and stays running, compiles "
                   <> "and gives quick feedback. "
                   <> "Similarly to quick needs an entrypoint. "
                   <> "To run tests use '-T EXPR' i.e. "
                   <> "mafia watch test/test.hs -- -T Test.Pure.tests" )
            (MafiaWatch <$> pGhciIncludes <*> pGhciEntryPoint <*> many pGhcidArgs)

 , command' "hoogle" ( "Run a hoogle query across the local dependencies" )
            (MafiaHoogle <$> many pCabalArgs)
 ]

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

update :: EitherT MafiaError IO ()
update = do
  home <- getHomeDirectory

  let index = home </> ".cabal/packages/hackage.haskell.org/00-index.cache"

  indexTime   <- getModificationTime index
  currentTime <- liftIO getCurrentTime

  let age    = currentTime `diffUTCTime` indexTime
      oneDay = 24 * 60 * 60

  when (age > oneDay) $
    firstEitherT MafiaCabalError $ cabal_ "update" []

build :: [Argument] -> EitherT MafiaError IO ()
build args = do
  initialize
  firstEitherT MafiaCabalError . cabal_ "build" $ ["--ghc-option=-Werror"] <> args

test :: [Argument] -> EitherT MafiaError IO ()
test args = do
  initialize
  firstEitherT MafiaCabalError . cabal_ "test" $ ["--show-details=streaming"] <> args

testci :: [Argument] -> EitherT MafiaError IO ()
testci args = do
  initialize
  Clean <- firstEitherT MafiaCabalError . cabal "test" $ ["--show-details=streaming"] <> args
  return ()

repl :: [Argument] -> EitherT MafiaError IO ()
repl args = do
  initialize
  firstEitherT MafiaCabalError $ cabal_ "repl" args

bench :: [Argument] -> EitherT MafiaError IO ()
bench args = do
  initialize
  firstEitherT MafiaCabalError $ cabal_ "bench" args

quick :: [GhciInclude] -> File -> EitherT MafiaError IO ()
quick extraIncludes path = do
  args <- ghciArgs extraIncludes path
  exec MafiaProcessError "ghci" args

watch :: [GhciInclude] -> File -> [Argument] -> EitherT MafiaError IO ()
watch extraIncludes path extraArgs = do
  ghcidExe <- firstEitherT MafiaProcessError $ installBinary "ghcid" "0.5" []
  args <- ghciArgs extraIncludes path
  exec MafiaProcessError ghcidExe $ [ "-c", T.unwords ("ghci" : args) ] <> extraArgs

ghciArgs :: [GhciInclude] -> File -> EitherT MafiaError IO [Argument]
ghciArgs extraIncludes path = do
  exists <- doesFileExist path
  case exists of
    False -> hoistEither (Left (MafiaEntryPointNotFound path))
    True  -> do
      initialize

      extras <- concat <$> mapM reifyInclude extraIncludes

      let dirs = ["src", "test", "gen", "dist/build/autogen"] <> extras
      includes  <- catMaybes <$> mapM ensureDirectory dirs
      databases <- getPackageDatabases

      return $ [ "-no-user-package-db" ]
            <> (fmap ("-i" <>)           includes)
            <> (fmap ("-package-db=" <>) databases)
            <> [ path ]

reifyInclude :: GhciInclude -> EitherT MafiaError IO [Directory]
reifyInclude = \case
  Directory dir -> return [dir]
  AllLibraries  -> do
    absDirs <- Set.toList <$> getSubmoduleSources
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
    sandboxDir <- firstEitherT MafiaCabalError initSandbox
    filter isPackage <$> getDirectoryListing Recursive sandboxDir
  where
    isPackage = ("-packages.conf.d" `T.isSuffixOf`)
