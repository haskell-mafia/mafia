{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           BuildInfo_ambiata_mafia

import           Control.Exception (IOException)
import           Control.Monad.Catch (MonadCatch(..), handle)
import           Control.Monad.IO.Class (MonadIO(..))

import qualified Crypto.Hash as Hash

import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import           Data.Time (getCurrentTime, diffUTCTime)

import           Mafia.Cabal
import           Mafia.Git
import           Mafia.IO
import           Mafia.Path
import           Mafia.Process

import           P

import           System.Environment (lookupEnv)
import           System.Exit (exitSuccess)
import           System.IO (BufferMode(..), hSetBuffering)
import           System.IO (IO, stdout, stderr, putStrLn, print)
import           System.IO.Temp (withTempDirectory)

import           X.Control.Monad.Trans.Either (EitherT, pattern EitherT, runEitherT)
import           X.Control.Monad.Trans.Either (hoistEither, firstEitherT)
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
        orDie renderViolation (run c)

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

run :: MafiaCommand -> EitherT MafiaViolation IO ()
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

data MafiaViolation
  = ProjectNotFound
  | MultipleProjectsFound [ProjectName]
  | CabalError CabalError
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

  CabalError (IndexFileNotFound file)
   -> "Index file not found: " <> file

  CabalError (CorruptIndexFile tarError)
   -> "Corrupt index file: " <> T.pack (show tarError)

  ProcessError (ProcessFailure p code)
   -> "Process failed: " <> T.intercalate " " (processCommand p : processArguments p)
   <> " (exit code: " <> T.pack (show code) <> ")"

  ProcessError (ProcessException p ex)
   -> "Process failed: " <> T.intercalate " " (processCommand p : processArguments p)
   <> "\n" <> T.pack (show ex)

  ParseError msg
   -> "Parse failed: " <> msg

  CacheUpdateError x ex
   -> "Cache update failed: " <> T.pack (show x)
   <> "\n" <> T.pack (show ex)

  EntryPointNotFound path
   -> "GHCi entry point not found: " <> path

  GhcidNotInstalled
   -> "ghcid is not installed."
   <> "\nTo install:"
   <> "\n - create a fresh cabal sandbox"
   <> "\n - cabal install ghcid"
   <> "\n - add to your $PATH"

liftGit :: Functor m => EitherT GitError m a -> EitherT MafiaViolation m a
liftGit = firstEitherT $ \case
  GitParseError   msg -> ParseError   msg
  GitProcessError err -> ProcessError err

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

bench :: [Argument] -> EitherT MafiaViolation IO ()
bench args = do
  initialize
  cabal_ "bench" args

quick :: [GhciInclude] -> File -> EitherT MafiaViolation IO ()
quick extraIncludes path = do
  args <- ghciArgs extraIncludes path
  exec ProcessError "ghci" args

watch :: [GhciInclude] -> File -> [Argument] -> EitherT MafiaViolation IO ()
watch extraIncludes path extraArgs = do
  Hush <- call (const GhcidNotInstalled) "ghcid" ["--help"]
  args <- ghciArgs extraIncludes path
  exec ProcessError "ghcid" $ [ "-c", T.unwords ("ghci" : args) ] <> extraArgs

ghciArgs :: [GhciInclude] -> File -> EitherT MafiaViolation IO [Argument]
ghciArgs extraIncludes path = do
  exists <- doesFileExist path
  case exists of
    False -> hoistEither (Left (EntryPointNotFound path))
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

reifyInclude :: GhciInclude -> EitherT MafiaViolation IO [Directory]
reifyInclude = \case
  Directory dir -> return [dir]
  AllLibraries  -> do
    absDirs <- Set.toList <$> getSubmoduleSources
    relDirs <- mapM tryMakeRelativeToCurrent absDirs
    return [ dir </> sub | dir <- relDirs
                         , sub <- ["src", "test", "gen", "dist/build/autogen"] ]

tryMakeRelativeToCurrent :: MonadIO m => Directory -> m Directory
tryMakeRelativeToCurrent dir =
  fromMaybe dir `liftM` makeRelativeToCurrentDirectory dir

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
    mapM_ runCacheUnregister sortedUpdates

    cabal_ "install" [ "-j"
                     , "--only-dependencies"
                     , "--force-reinstalls"
                     , "--enable-relocatable"
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
  = Add    File File
  | Update File File
  | Delete File
  deriving (Eq, Ord, Show)

determineCacheUpdates :: EitherT MafiaViolation IO [CacheUpdate]
determineCacheUpdates = do
    liftGit initSubmodules
    syncCabalSources

    currentDir  <- getCurrentDirectory
    sandboxSrcs <- Set.toList <$> getSandboxSources
    let allSrcs = currentDir : sandboxSrcs

    srcs     <- mkFileMap . concat <$> mapM findCabal allSrcs
    cacheDir <- getCacheDir
    dsts     <- mkFileMap <$> getDirectoryListing (RecursiveDepth 0) cacheDir

    let mkAdd src = Add src (cacheDir </> takeFileName src)

        fresh = fmap mkAdd  (Map.elems (srcs `Map.difference` dsts))
        stale = fmap Delete (Map.elems (dsts `Map.difference` srcs))

    updates <- sequence (Map.elems (Map.intersectionWith cacheUpdate srcs dsts))

    return (stale <> fresh <> catMaybes updates)

findCabal :: MonadIO m => Directory -> m [Path]
findCabal dir = filter (extension ".cabal")
        `liftM` getDirectoryListing (RecursiveDepth 0) dir

mkFileMap :: [Path] -> Map File Path
mkFileMap = Map.fromList . fmap (\path -> (takeFileName path, path))

putUpdateReason :: CacheUpdate -> EitherT MafiaViolation IO ()
putUpdateReason x =
  case x of
    Add src _ -> do
      rel <- fromMaybe src <$> makeRelativeToCurrentDirectory src
      liftIO (T.hPutStrLn stderr ("Cache: Adding " <> rel))

    Update src _ -> do
      rel <- fromMaybe src <$> makeRelativeToCurrentDirectory src
      liftIO (T.hPutStrLn stderr ("Cache: Updating " <> rel))

    Delete file -> do
      liftIO (T.hPutStrLn stderr ("Cache: Removing " <> takeFileName file))

runCacheUpdate :: CacheUpdate -> EitherT MafiaViolation IO ()
runCacheUpdate x = handleCacheUpdateError x $
  case x of
    Add src dst -> do
      copyFile src dst

    Update src dst -> do
      copyFile src dst

    Delete file -> do
      removeFile file

handleCacheUpdateError :: MonadCatch m
                       => CacheUpdate
                       -> EitherT MafiaViolation m a
                       -> EitherT MafiaViolation m a

handleCacheUpdateError x =
  handle (\(ex :: IOException) -> hoistEither . Left $ CacheUpdateError x ex)

runCacheUnregister :: CacheUpdate -> EitherT MafiaViolation IO ()
runCacheUnregister x =
  case x of
    Add src dst -> do
      tryUnregisterPackage src
      tryUnregisterPackage dst
      invalidateCache dst

    Update src dst -> do
      tryUnregisterPackage src
      tryUnregisterPackage dst
      invalidateCache dst

    Delete file -> do
      tryUnregisterPackage file
      invalidateCache file

tryUnregisterPackage :: MonadIO m => File -> m ()
tryUnregisterPackage cabalFile = do
  mpkg <- readPackageName cabalFile
  case mpkg of
    Nothing  -> return ()
    Just pkg -> do
      -- This is only best effort, if unregistering fails it means we've probably
      -- already unregistered the package or it was never registered, no harm is
      -- done because we'll be reinstalling it later if it's required.

      result <- liftIO . runEitherT $ do
        Hush <- cabal "sandbox" ["hc-pkg", "--", "unregister", "--force", pkg]
        return ()

      case result of
        Left  _ -> return ()
        Right _ -> liftIO (T.putStrLn ("Sandbox: Unregistered " <> pkg))

invalidateCache :: MonadIO m => File -> m ()
invalidateCache cabalFile = do
  mtxt <- readUtf8 cabalFile
  case mtxt of
    Nothing  -> return ()
    Just txt -> do
      time <- liftIO getCurrentTime
      let header = "-- " <> T.pack (show time) <> "\n"
      writeUtf8 cabalFile (header <> txt)

cacheUpdate :: File -> File -> EitherT MafiaViolation IO (Maybe CacheUpdate)
cacheUpdate src dst = do
  src_missing <- not <$> doesFileExist src
  dst_missing <- not <$> doesFileExist dst
  if | src_missing && dst_missing -> return (Nothing)
     | src_missing                -> return (Just (Delete dst))
     | dst_missing                -> return (Just (Update src dst))
     | otherwise                  -> cacheUpdateDiff src dst

cacheUpdateDiff :: File -> File -> EitherT MafiaViolation IO (Maybe CacheUpdate)
cacheUpdateDiff src dst = do
  src_bytes <- readBytes src
  dst_bytes <- readBytes dst
  if | src_bytes == dst_bytes -> return (Nothing)
     | otherwise              -> return (Just (Update src dst))

------------------------------------------------------------------------

syncCabalSources :: EitherT MafiaViolation IO ()
syncCabalSources = do
  repairSandbox
  installed <- getSandboxSources
  required  <- getSubmoduleSources
  traverse_ addSandboxSource    (required `Set.difference` installed)
  traverse_ removeSandboxSource (installed `Set.difference` required)

repairSandbox :: EitherT MafiaViolation IO ()
repairSandbox = do
  sandboxDir <- getSandboxDir
  firstEitherT CabalError (repairIndexFile sandboxDir)

addSandboxSource :: Directory -> EitherT MafiaViolation IO ()
addSandboxSource dir = do
  rel <- fromMaybe dir <$> makeRelativeToCurrentDirectory dir
  liftIO (T.hPutStrLn stderr ("Sandbox: Adding " <> rel))
  sandbox_ "add-source" [dir]

removeSandboxSource :: Directory -> EitherT MafiaViolation IO ()
removeSandboxSource dir = do
  rel <- fromMaybe dir <$> makeRelativeToCurrentDirectory dir
  liftIO (T.hPutStrLn stderr ("Sandbox: Removing " <> rel))
  sandbox_ "delete-source" ["-v0", dir]

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
  root <- liftGit getProjectRoot
  name <- getProjectName
  cfg  <- readUtf8 (name <> ".submodules")
  return . Set.fromList
         . fmap (root </>)
         . T.lines
         . fromMaybe T.empty
         $ cfg

getConventionSources :: EitherT MafiaViolation IO (Set Directory)
getConventionSources = do
  root <- liftGit getProjectRoot
  subs <- fmap ((root </>) . subName) <$> liftGit getSubmodules
  Set.unions <$> traverse getSourcesFrom subs

getSourcesFrom :: MonadIO m => Directory -> m (Set Path)
getSourcesFrom dir = do
  let cleanDir x = dropTrailingPathSeparator `liftM` canonicalizePath x

  dir'    <- cleanDir dir
  entries <- getDirectoryListing (RecursiveDepth 3) dir'
  sources <- mapM cleanDir
           . fmap   (dir' </>)
           . fmap   (takeDirectory)
           . filter (not . T.isInfixOf ".cabal-sandbox/")
           . filter (not . T.isPrefixOf "lib/")
           . filter (not . T.isPrefixOf "bin/")
           . filter (extension ".cabal")
           . fmap   (T.drop (T.length dir' + 1))
           $ entries

  return (Set.fromList sources)

------------------------------------------------------------------------

-- Sandbox initialized if required, this should support sandboxes in parent
-- directories.
getSandboxDir :: EitherT MafiaViolation IO Directory
getSandboxDir = do
  name <- getProjectName

  sandboxDir <- fromMaybe ".cabal-sandbox"
            <$> liftIO (readUtf8 (name <> ".sandbox"))

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

getMafiaHome :: IO Text
getMafiaHome =
  (</> T.pack ".ambiata/mafia") <$> getHomeDirectory

ensureMafiaDir :: MonadIO m => Text -> m Text
ensureMafiaDir path = liftIO $ do
  home <- getMafiaHome
  let path' = home </> path
  createDirectoryIfMissing True path'
  pure path'

hoogle :: Text -> [Argument] -> EitherT MafiaViolation IO ()
hoogle hackageRoot args = do
  initialize
  db <- ensureMafiaDir "hoogle"
  hoogleExe <- installBinary "hoogle" "4.2.43" [("happy", "1.19.5")]
  Out pkgStr <- sandbox "hc-pkg" ["list"]
  let pkgs = fmap T.strip . filter (T.isPrefixOf " ") . T.lines $ pkgStr
  hoos <- fmap catMaybes . for pkgs $ \pkg -> do
    -- Extract name from `$name-$version`, but consider `unordered-containers-1.2.3`
    let name = T.intercalate "-" . List.init $ T.splitOn "-" pkg
    let txt = db </> pkg <> ".txt"
    let hoo = db </> pkg <> ".hoo"
    let skip = db </> pkg <> ".skip"
    ifM (doesFileExist skip) (pure Nothing) $
      ifM (doesFileExist hoo) (pure $ Just hoo) $ do
        liftIO . T.hPutStrLn stderr $ "Downloading: " <> pkg
        r <- runEitherT $ call ProcessError "curl" ["-f", "-s", hackageRoot </> pkg </> "docs" </> name <> ".txt", "-o", txt]
        case r of
          Left _ -> do
            liftIO . T.hPutStrLn stderr $ "Missing: " <> pkg
            -- Technically we can "convert" a broken txt file and no one is the wiser, but we're not going to do that
            liftIO $ T.writeFile (T.unpack skip) ""
            pure Nothing
          Right Hush -> do
            call_ ProcessError hoogleExe ["convert", txt]
            pure $ Just hoo
  -- By default hoogle will expect a 'default.hoo' file to exist in the database directory
  -- If we want the search to just be for _this_ sandbox, we have two options
  -- 1. Create a unique directory based on all the current packages and ensure the default.hoo
  -- 2. Specify/append all the packages from the global database by using "+$name-$version"
  --    Unfortunately hoogle doesn't like the "-$version" part :(
  let hash = T.decodeUtf8 . (\(d :: Hash.Digest Hash.SHA1) -> Hash.digestToHexByteString d) . Hash.hash . T.encodeUtf8 . mconcat $ pkgs
  db' <- (\d -> d </> "hoogle" </> hash) <$> getSandboxDir
  unlessM (doesFileExist $ db' </> "default.hoo") $ do
    createDirectoryIfMissing True db'
    -- We may also want to copy/symlink all the hoo files here to allow for partial module searching
    call_ ProcessError hoogleExe $ ["combine", "--outfile", db' </> "default.hoo"] <> hoos
  call_ ProcessError hoogleExe $ ["-d", db'] <> args

-- | Installs a given cabal package at a specific version
installBinary :: Text -> Text -> [(Text, Text)] -> EitherT MafiaViolation IO File
installBinary name version deps = do
  tmp <- ensureMafiaDir "tmp"
  let nv = name <> "-" <> version
  bin <- ensureMafiaDir "bin"
  let path = bin </> nv
  liftIO (doesFileExist path) >>= \case
    True ->
      pure path
    False -> do
      EitherT . withTempDirectory (T.unpack tmp) (T.unpack $ nv <> ".") $ \sandboxDir -> runEitherT $ do
        Pass <- callFrom ProcessError (T.pack sandboxDir) "cabal" ["sandbox", "init"]
        -- Install any required executables first
        -- This could also recursively call `installBinary` and copy the output in to the sandbox if we do this again
        for_ deps $ \(n, v) -> do
          Pass <- callFrom ProcessError (T.pack sandboxDir) "cabal" ["install", n <> "-" <> v]
          pure ()
        Pass <- callFrom ProcessError (T.pack sandboxDir) "cabal" ["install", nv]
        copyFile (T.pack sandboxDir </> ".cabal-sandbox" </> "bin" </> name) path
        return path
