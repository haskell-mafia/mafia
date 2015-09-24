{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           BuildInfo_ambiata_mafia

import           Control.Monad.IO.Class (liftIO)

import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Mafia.IO
import           Mafia.Path
import           Mafia.Process

import           Options.Applicative

import           P

import           System.Exit
import           System.IO

import           X.Options.Applicative
import           X.Control.Monad.Trans.Either

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
        orDie renderViolation (run c)

------------------------------------------------------------------------

data MafiaCommand
  = MafiaCommand
  deriving (Eq, Show)

parser :: Parser (SafeCommand MafiaCommand)
parser =
  safeCommand $ pure MafiaCommand

run :: MafiaCommand -> EitherT MafiaViolation IO ()
run c = case c of
  MafiaCommand -> initialize

------------------------------------------------------------------------

data MafiaViolation
  = ProjectNotFound
  | MultipleProjectsFound [ProjectName]
  | ProcessError ProcessError
  | ParseError Text
  deriving (Eq, Show)

renderViolation :: MafiaViolation -> Text
renderViolation = \case
  ProjectNotFound
   -> "Could not find .cabal project"

  MultipleProjectsFound ps
   -> "Found multiple possible .cabal projects: "
   <> T.intercalate ", " ps

  ProcessError (ProcessFailure p code)
   -> "Process failed: " <> T.intercalate " " (processCommand p : processArguments p)
   <> " (exit code: " <> T.pack (show code) <> ")"

  ProcessError (ProcessException p ex)
   -> "Process failed: " <> T.intercalate " " (processCommand p : processArguments p)
   <> "\n" <> T.pack (show ex)

  ParseError msg
   -> "Parse failed: " <> msg

------------------------------------------------------------------------

-- Initialize things for a build. This can be made faster by being
-- a lot smarter about doing things conditionally, but for now,
-- brute force wins.
initialize :: EitherT MafiaViolation IO ()
initialize = do
    initSubmodules
    syncCabalSources

    cabal_ "install" [ "-j"
                     , "--only-dependencies"
                     , "--force-reinstalls"
                     , "--enable-tests"
                     , "--enable-benchmarks" ]

    cabal_ "configure" [ "--enable-tests"
                       , "--enable-benchmarks" ]

------------------------------------------------------------------------

type ProjectName = Text

getProjectRoot :: EitherT MafiaViolation IO File
getProjectRoot =
  T.strip . unOut <$> call ProcessError "git" ["rev-parse", "--show-toplevel"]

getProjectName :: EitherT MafiaViolation IO ProjectName
getProjectName = EitherT $ do
  entries <- getDirectoryContents "."

  let projects = fmap dropExtension
               . filter ((== ".cabal") . takeExtension)
               $ entries

  case projects of
    []  -> return (Left ProjectNotFound)
    [p] -> return (Right p)
    ps  -> return (Left (MultipleProjectsFound ps))

------------------------------------------------------------------------

syncCabalSources :: EitherT MafiaViolation IO ()
syncCabalSources = do
  installed <- getSandboxSources
  required  <- getSubmoduleSources
  addSandboxSources    (required `Set.difference` installed)
  removeSandboxSources (installed `Set.difference` required)

addSandboxSources :: Set Directory -> EitherT MafiaViolation IO ()
addSandboxSources = mapM_ add . Set.toList
  where
    add dir = do
      liftIO (T.hPutStrLn stderr ("Adding " <> dir))
      sandbox_ "add-source" [dir]

removeSandboxSources :: Set Directory -> EitherT MafiaViolation IO ()
removeSandboxSources = mapM_ delete . Set.toList
  where
    delete dir = do
      -- TODO Unregister packages contained in this source dir
      liftIO (T.hPutStrLn stderr ("Removing " <> dir))
      sandbox_ "delete-source" [dir]

getSandboxSources :: EitherT MafiaViolation IO (Set Directory)
getSandboxSources = do
  Out sources <- sandbox "list-sources" []
  return . Set.fromList
         . reverse
         . drop 2
         . reverse
         . drop 3
         . T.lines
         $ sources

getSubmoduleSources :: EitherT MafiaViolation IO (Set Directory)
getSubmoduleSources = Set.union <$> getConfiguredSources
                                <*> getConventionSources

getConfiguredSources :: EitherT MafiaViolation IO (Set Directory)
getConfiguredSources = do
  root <- getProjectRoot
  name <- getProjectName
  cfg  <- readText (name <> ".submodules")
  return . Set.fromList
         . fmap (root </>)
         . T.lines
         . fromMaybe T.empty
         $ cfg

getConventionSources :: EitherT MafiaViolation IO (Set Directory)
getConventionSources = do
  root <- getProjectRoot

  let lib = root </> "lib/"
  entries <- getDirectoryListing (RecursiveDepth 4) lib

  return . Set.fromList
         . fmap   (lib </>)
         . fmap   (takeDirectory)
         . filter (not . T.isInfixOf "/lib/")
         . filter (not . T.isInfixOf "/bin/")
         . filter (extension ".cabal")
         . fmap   (T.drop (T.length lib))
         $ entries

------------------------------------------------------------------------

-- Sandbox initialized if required, this should support sandboxes in parent
-- directories.
initSandbox :: EitherT MafiaViolation IO File
initSandbox = do
  name <- getProjectName

  sandboxDir <- fromMaybe ".cabal-sandbox"
            <$> liftIO (readText (name <> ".sandbox"))

  showtime <- liftIO (doesDirectoryExist sandboxDir)
  unless showtime $
    call_ ProcessError "cabal" ["sandbox", "--sandbox", sandboxDir, "init"]

  return sandboxDir

------------------------------------------------------------------------

cabal :: ProcessResult a => Argument -> [Argument] -> EitherT MafiaViolation IO a
cabal cmd args = call ProcessError "cabal" (cmd : args)

cabal_ :: Argument -> [Argument] -> EitherT MafiaViolation IO ()
cabal_ cmd args = do
  Pass <- cabal cmd args
  return ()

sandbox :: ProcessResult a => Argument -> [Argument] -> EitherT MafiaViolation IO a
sandbox cmd args = do
  sandboxDir <- initSandbox
  cabal "sandbox" $ ["--sandbox", sandboxDir] <> (cmd:args)

sandbox_ :: Argument -> [Argument] -> EitherT MafiaViolation IO ()
sandbox_ cmd args = do
  Pass <- sandbox cmd args
  return ()

------------------------------------------------------------------------

data SubmoduleState = NeedsInit | Ready
  deriving (Eq, Ord, Show)

data Submodule = Submodule
  { subState :: SubmoduleState
  , subName  :: Path
  } deriving (Eq, Ord, Show)

subNeedsInit :: Submodule -> Bool
subNeedsInit = (== NeedsInit) . subState

-- Init any submodules that we need. Don't worry about explicit submodules
-- file here, we just want to trust git to tell us things that haven't been
-- initialized, we really _don't_ want to run this just because a module is
-- dirty from development changes etc...
initSubmodules :: EitherT MafiaViolation IO ()
initSubmodules = do
  root <- getProjectRoot
  ss   <- fmap subName . filter subNeedsInit <$> getSubmodules

  forM_ ss $ \s ->
    callFrom_ ProcessError root "git" ["submodule", "update", "--init", s]

getSubmodules :: EitherT MafiaViolation IO [Submodule]
getSubmodules = do
    root    <- getProjectRoot
    Out out <- callFrom ProcessError root "git" ["submodule"]

    sequence . fmap parseLine
             . T.lines
             $ out
  where
    parseLine line = Submodule <$> parseState line
                               <*> parseName  line

    parseState line
      | "-" `T.isPrefixOf` line = pure NeedsInit
      | otherwise               = pure Ready

    parseName line =
      case T.words line of
        (_:x:_) -> pure x
        _       -> left (ParseError ("failed to read submodule name from: " <> line))
