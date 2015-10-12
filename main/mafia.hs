{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           BuildInfo_ambiata_mafia

import           Control.Exception (IOException)
import           Control.Monad.Catch (handle)
import           Control.Monad.IO.Class (MonadIO(..))

import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Time (getCurrentTime, diffUTCTime)

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
  = MafiaUpdate
  | MafiaBuild  [Argument]
  | MafiaTest   [Argument]
  | MafiaTestCI [Argument]
  | MafiaRepl   [Argument]
  | MafiaQuick  File
  | MafiaWatch  File [Argument]
  deriving (Eq, Show)

run :: MafiaCommand -> EitherT MafiaViolation IO ()
run = \case
  MafiaUpdate            -> update
  MafiaBuild  args       -> build  args
  MafiaTest   args       -> test   args
  MafiaTestCI args       -> testci args
  MafiaRepl   args       -> repl   args
  MafiaQuick  entry      -> quick  entry
  MafiaWatch  entry args -> watch  entry args

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

 , command' "quick" ( "Start the repl directly skipping cabal, this is useful "
                   <> "developing across multiple source trees at once." )
            (MafiaQuick <$> pGhciEntryPoint)

 , command' "watch" ( "Watches filesystem for changes and stays running, compiles "
                   <> "and gives quick feedback. "
                   <> "Similarly to quick needs an entrypoint. "
                   <> "To run tests use '-T EXPR' i.e. "
                   <> "mafia watch test/test.hs -- -T Test.Pure.tests" )
            (MafiaWatch <$> pGhciEntryPoint <*> many pGhcidArgs)
 ]

pGhciEntryPoint :: Parser File
pGhciEntryPoint =
  argument textRead $
       metavar "FILE"
    <> help "The entry point for GHCi."

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

data MafiaViolation
  = ProjectNotFound
  | MultipleProjectsFound [ProjectName]
  | ProcessError ProcessError
  | ParseError Text
  | CacheUpdateError CacheUpdate IOException
  | EntryPointNotFound File
  | GhcidNotInstalled
  deriving (Show)

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

  CacheUpdateError sync ex
   -> "Cache sync failed: " <> T.pack (show sync)
   <> "\n" <> T.pack (show ex)

  EntryPointNotFound path
   -> "GHCi entry point not found: " <> path

  GhcidNotInstalled
   -> "ghcid is not installed."
   <> "\nTo install:"
   <> "\n - create a fresh cabal sandbox"
   <> "\n - cabal install ghcid"
   <> "\n - add to your $PATH"

------------------------------------------------------------------------

update :: EitherT MafiaViolation IO ()
update = do
  home <- getHomeDirectory

  let index = home </> ".cabal/packages/hackage.haskell.org/00-index.cache"

  indexTime   <- getModificationTime index
  currentTime <- liftIO getCurrentTime

  let age    = currentTime `diffUTCTime` indexTime
      oneDay = 24 * 60 * 60

  when (age > oneDay) (cabal_ "update" [])

build :: [Argument] -> EitherT MafiaViolation IO ()
build args = do
  initialize
  cabal_ "build" $ ["--ghc-option=-Werror"] <> args

test :: [Argument] -> EitherT MafiaViolation IO ()
test args = do
  initialize
  cabal_ "test" $ ["--show-details=streaming"] <> args

testci :: [Argument] -> EitherT MafiaViolation IO ()
testci args = do
  initialize
  Clean <- cabal "test" $ ["--show-details=streaming"] <> args
  return ()

repl :: [Argument] -> EitherT MafiaViolation IO ()
repl args = do
  initialize
  cabal_ "repl" args

quick :: File -> EitherT MafiaViolation IO ()
quick path = do
  args <- ghciArgs path
  exec ProcessError "ghci" args

watch :: File -> [Argument] -> EitherT MafiaViolation IO ()
watch path extraArgs = do
  Hush <- call (const GhcidNotInstalled) "ghcid" ["--help"]
  args <- ghciArgs path
  exec ProcessError "ghcid" $ [ "-c", T.unwords ("ghci" : args) ] <> extraArgs

ghciArgs :: File -> EitherT MafiaViolation IO [Argument]
ghciArgs path = do
  exists <- doesFileExist path
  case exists of
    False -> hoistEither (Left (EntryPointNotFound path))
    True  -> do
      initialize

      let dirs = ["src", "test", "gen", "dist/build/autogen"]
      includes  <- catMaybes <$> mapM ensureDirectory dirs
      databases <- getPackageDatabases

      return $ [ "-no-user-package-db" ]
            <> (fmap ("-i" <>)           includes)
            <> (fmap ("-package-db=" <>) databases)
            <> [ path ]

ensureDirectory :: MonadIO m => Directory -> m (Maybe Directory)
ensureDirectory dir = do
  exists <- doesDirectoryExist dir
  case exists of
    False -> return Nothing
    True  -> return (Just dir)

------------------------------------------------------------------------

-- Initialize things for a build. This can be made faster by being
-- a lot smarter about doing things conditionally, but for now,
-- brute force wins.
initialize :: EitherT MafiaViolation IO ()
initialize = do
  updates <- determineCacheUpdates
  when (updates /= []) $ do
    -- we want to know up front why we're doing an install/configure
    let sortedUpdates = List.sort updates
    mapM_ putUpdateReason sortedUpdates

    cabal_ "install" [ "-j"
                     , "--only-dependencies"
                     , "--force-reinstalls"
                     , "--enable-tests"
                     , "--enable-benchmarks"
                     , "--reorder-goals"
                     , "--max-backjumps=-1" ]

    cabal_ "configure" [ "--enable-tests"
                       , "--enable-benchmarks" ]

    -- but we don't want to commit the modified .cabal files
    -- until we're done, in case an error occurs
    mapM_ runCacheUpdate sortedUpdates

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

data CacheUpdate
  = Delete File
  | Update File File
  deriving (Eq, Ord, Show)

determineCacheUpdates :: EitherT MafiaViolation IO [CacheUpdate]
determineCacheUpdates = do
    initSubmodules
    syncCabalSources

    currentDir  <- getCurrentDirectory
    sandboxSrcs <- Set.toList <$> getSandboxSources
    let allSrcs = currentDir : sandboxSrcs

    srcs     <- mkFileMap . concat <$> mapM findCabal allSrcs
    cacheDir <- getCacheDir
    dsts     <- mkFileMap <$> getDirectoryListing (RecursiveDepth 0) cacheDir

    let mkUpdate src = Update src (cacheDir </> takeFileName src)

        stale = fmap Delete   (Map.elems (dsts `Map.difference` srcs))
        fresh = fmap mkUpdate (Map.elems (srcs `Map.difference` dsts))

    updates <- sequence (Map.elems (Map.intersectionWith cacheUpdate srcs dsts))

    return (stale <> fresh <> catMaybes updates)

findCabal :: MonadIO m => Directory -> m [Path]
findCabal dir = filter (extension ".cabal")
        `liftM` getDirectoryListing (RecursiveDepth 0) dir

mkFileMap :: [Path] -> Map File Path
mkFileMap = Map.fromList . fmap (\path -> (takeFileName path, path))

putUpdateReason :: CacheUpdate -> EitherT MafiaViolation IO ()
putUpdateReason sync =
  case sync of
    Delete file
     -> do liftIO (T.hPutStrLn stderr ("Cache: Removed " <> takeFileName file))

    Update src _
     -> do rel <- fromMaybe src <$> makeRelativeToCurrentDirectory src
           liftIO (T.hPutStrLn stderr ("Cache: Modified " <> rel))

runCacheUpdate :: CacheUpdate -> EitherT MafiaViolation IO ()
runCacheUpdate sync = handle onError $
  case sync of
    Delete file    -> removeFile file
    Update src dst -> copyFile src dst
  where
    onError (ex :: IOException) = hoistEither (Left (CacheUpdateError sync ex))

cacheUpdate :: File -> File -> EitherT MafiaViolation IO (Maybe CacheUpdate)
cacheUpdate src dst = do
  src_exist <- doesFileExist src
  dst_exist <- doesFileExist dst
  case (src_exist, dst_exist) of
    (False, False) -> return (Nothing)
    (False, True)  -> return (Just (Delete dst))
    (True,  False) -> return (Just (Update src dst))
    (True,  True)  -> do
      src_time  <- getModificationTime src
      dst_time  <- getModificationTime dst
      case src_time == dst_time of
        True  -> return (Nothing)
        False -> do
          src_bytes <- readBytes src
          dst_bytes <- readBytes dst
          case src_bytes == dst_bytes of
            True  -> return (Nothing)
            False -> return (Just (Update src dst))

------------------------------------------------------------------------

syncCabalSources :: EitherT MafiaViolation IO ()
syncCabalSources = do
  installed <- getSandboxSources
  required  <- getSubmoduleSources
  traverse_ addSandboxSource    (required `Set.difference` installed)
  traverse_ removeSandboxSource (installed `Set.difference` required)

addSandboxSource :: Directory -> EitherT MafiaViolation IO ()
addSandboxSource dir = do
  rel <- fromMaybe dir <$> makeRelativeToCurrentDirectory dir
  liftIO (T.hPutStrLn stderr ("Sandbox: Adding " <> rel))
  sandbox_ "add-source" [dir]

removeSandboxSource :: Directory -> EitherT MafiaViolation IO ()
removeSandboxSource dir = do
  rel <- fromMaybe dir <$> makeRelativeToCurrentDirectory dir
  liftIO (T.hPutStrLn stderr ("Sandbox: Removing " <> rel))
  -- TODO Unregister packages contained in this source dir
  sandbox_ "delete-source" [dir]

getSandboxSources :: EitherT MafiaViolation IO (Set Directory)
getSandboxSources = do
  Out sources <- sandbox "list-sources" []

  let dropHeader = drop 3
      dropFooter = reverse . drop 2 . reverse

  return . Set.fromList
         . dropFooter
         . dropHeader
         . T.lines
         $ sources

getSubmoduleSources :: EitherT MafiaViolation IO (Set Directory)
getSubmoduleSources = Set.union <$> getConfiguredSources
                                <*> getConventionSources

getConfiguredSources :: EitherT MafiaViolation IO (Set Directory)
getConfiguredSources = do
  name  <- getProjectName
  cfg   <- readText (name <> ".submodules")

  paths <- traverse canonicalizePath
         . T.lines
         . fromMaybe T.empty
         $ cfg

  return (Set.fromList paths)

getConventionSources :: EitherT MafiaViolation IO (Set Directory)
getConventionSources = do
  root <- getProjectRoot

  let lib = root </> "lib/"
  exists <- doesDirectoryExist lib

  case exists of
    False -> return Set.empty
    True  -> do
      entries <- getDirectoryListing (RecursiveDepth 4) lib
      return . Set.fromList
             . fmap   (lib </>)
             . fmap   (takeDirectory)
             . filter (not . T.isInfixOf ".cabal-sandbox")
             . filter (not . T.isInfixOf "/lib/")
             . filter (not . T.isInfixOf "/bin/")
             . filter (extension ".cabal")
             . fmap   (T.drop (T.length lib))
             $ entries

------------------------------------------------------------------------

-- Sandbox initialized if required, this should support sandboxes in parent
-- directories.
getSandboxDir :: EitherT MafiaViolation IO Directory
getSandboxDir = do
  name <- getProjectName

  sandboxDir <- fromMaybe ".cabal-sandbox"
            <$> liftIO (readText (name <> ".sandbox"))

  showtime <- liftIO (doesDirectoryExist sandboxDir)
  unless showtime $
    call_ ProcessError "cabal" ["sandbox", "--sandbox", sandboxDir, "init"]

  return sandboxDir

getPackageDatabases :: EitherT MafiaViolation IO [Directory]
getPackageDatabases = do
    sandboxDir <- getSandboxDir
    filter isPackage <$> getDirectoryListing Recursive sandboxDir
  where
    isPackage = ("-packages.conf.d" `T.isSuffixOf`)

getCacheDir :: EitherT MafiaViolation IO Directory
getCacheDir = do
  cacheDir <- (</> "mafia") <$> getSandboxDir
  createDirectoryIfMissing False cacheDir
  return cacheDir

------------------------------------------------------------------------

cabal :: ProcessResult a => Argument -> [Argument] -> EitherT MafiaViolation IO a
cabal cmd args = call ProcessError "cabal" (cmd : args)

cabal_ :: Argument -> [Argument] -> EitherT MafiaViolation IO ()
cabal_ cmd args = do
  Pass <- cabal cmd args
  return ()

sandbox :: ProcessResult a => Argument -> [Argument] -> EitherT MafiaViolation IO a
sandbox cmd args = do
  sandboxDir <- getSandboxDir
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

  sequence . fmap parseSubmoduleLine
           . T.lines
           $ out

parseSubmoduleLine :: Text -> EitherT MafiaViolation IO Submodule
parseSubmoduleLine line = Submodule (parseSubmoduleState line) <$> parseSubmoduleName line

parseSubmoduleState :: Text -> SubmoduleState
parseSubmoduleState line
  | "-" `T.isPrefixOf` line = NeedsInit
  | otherwise               = Ready

parseSubmoduleName :: Text -> EitherT MafiaViolation IO Text
parseSubmoduleName line =
  case T.words line of
    (_:x:_) -> pure x
    _       -> left (ParseError ("failed to read submodule name from: " <> line))
