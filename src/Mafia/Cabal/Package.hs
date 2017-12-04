{-# LANGUAGE CPP #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Mafia.Cabal.Package (
    BuildTool(..)
  , getBuildTools

  , PackageType(..)
  , getPackageId
  , getPackageType
  , getCabalFile
  , findCabalRoot

  , SourcePackage(..)
  , getSourcePackage

  , SourcePackageHash(..)
  , renderSourcePackageHash
  , hashSourcePackage
  ) where

import           Control.Monad.IO.Class (MonadIO(..))

import qualified Data.List as List
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T

import qualified Distribution.Package as Cabal (PackageIdentifier(..))
import qualified Distribution.Package as Cabal (unPackageName)
import           Distribution.PackageDescription (BuildInfo(..))
import           Distribution.PackageDescription (CondTree(..))
import           Distribution.PackageDescription (Executable(..))
import           Distribution.PackageDescription (GenericPackageDescription(..))
import           Distribution.PackageDescription (libBuildInfo)
import qualified Distribution.PackageDescription as Cabal (GenericPackageDescription(..))
import qualified Distribution.PackageDescription as Cabal (Library(..))
import qualified Distribution.PackageDescription as Cabal (PackageDescription(..))
import           Distribution.PackageDescription.Parse (ParseResult(..))

#if MIN_VERSION_Cabal(2,0,0)
import           Distribution.PackageDescription.Parse (parseGenericPackageDescription)
import           Distribution.Types.CondTree (CondBranch (..))
import           Distribution.Types.LegacyExeDependency (LegacyExeDependency(..))
#else
import           Distribution.Package (Dependency(..))
import           Distribution.PackageDescription.Parse (parsePackageDescription)
#endif

import           Distribution.Version (VersionRange, anyVersion, asVersionIntervals)
import qualified Distribution.Version as Cabal (LowerBound(..), UpperBound(..), Bound(..), Version)

import           Mafia.Cabal.Constraint
import           Mafia.Cabal.Types
import           Mafia.Hash
import           Mafia.IO
import           Mafia.Package
import           Mafia.Path
import           Mafia.Process

import           P

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT, left, runEitherT)

------------------------------------------------------------------------

getCabalFile :: Directory -> EitherT CabalError IO File
getCabalFile dir = do
  xs <- filterM isCabalFile =<< getDirectoryListing (RecursiveDepth 0) dir
  case xs of
    [] ->
      left $ CabalFileNotFound dir
    x : [] ->
      return x
    _ : _ ->
      left $ CabalMultipleFilesFound dir xs

-- | Given a file or directory path, try to find the .cabal file by searching
--   its parent directories.
findCabalRoot :: MonadIO m => Path -> m (Maybe Directory)
findCabalRoot path0 = do
  path <- canonicalizePath path0
  entries <- tryGetDirectoryContents path
  if any (\x -> extension ".cabal" x && x /= ".cabal" && not (T.isSuffixOf "-test.cabal" x)) entries then
    return $ Just path
  else if path == "/" then
    return Nothing
  else
    findCabalRoot $ takeDirectory path

tryGetDirectoryContents :: MonadIO m => Path -> m [File]
tryGetDirectoryContents path = do
  isDir <- doesDirectoryExist path
  if isDir then
    getDirectoryContents path
  else
    return []

isCabalFile :: MonadIO m => File -> m Bool
isCabalFile file =
  if takeExtension file == ".cabal" && not (T.isPrefixOf "." file) then
    doesFileExist file
  else
    return False

withCabalFile :: Directory -> (File -> EitherT CabalError IO a) -> EitherT CabalError IO a
withCabalFile dir io = do
  io =<< getCabalFile dir

------------------------------------------------------------------------

data BuildTool =
  BuildTool {
      toolName :: PackageName
    , toolConstraints :: [Constraint]
    } deriving (Eq, Ord, Show)

getBuildTools :: Directory -> EitherT CabalError IO (Set BuildTool)
getBuildTools dir =
  withCabalFile dir $ \file -> do
    msrc <- fmap T.unpack <$> readUtf8 file
#if MIN_VERSION_Cabal(2,0,0)
    let parsePackageDescription =
          parseGenericPackageDescription
#endif
    case msrc of
      Nothing ->
        left $ CabalCouldNotReadBuildTools file
      Just src ->
        case parsePackageDescription src of
          ParseFailed err ->
            left $ CabalFileParseError file err
          ParseOk _ gpd ->
            pure $ takeTools gpd

takeTools :: GenericPackageDescription -> Set BuildTool
takeTools gpd =
  let
    lib =
      fromMaybe Set.empty . fmap (toolsOfCondTree toolsOfLibrary) $ condLibrary gpd

    exes =
      fmap (toolsOfCondTree toolsOfExecutable . snd) $ condExecutables gpd
  in
    Set.unions $ lib : exes

toolsOfCondTree :: (a -> Set BuildTool) -> CondTree v c a -> Set BuildTool
toolsOfCondTree f tree =
  let
#if MIN_VERSION_Cabal(2,0,0)
    loop (CondBranch _ x my) =
#else
    loop (_, x, my) =
#endif
      toolsOfCondTree f x
        <> maybe Set.empty (toolsOfCondTree f) my
  in
    Set.unions $
      f (condTreeData tree) : fmap loop (condTreeComponents tree)

toolsOfLibrary :: Cabal.Library -> Set BuildTool
toolsOfLibrary =
  toolsOfBuildInfo . libBuildInfo

toolsOfExecutable :: Executable -> Set BuildTool
toolsOfExecutable =
  toolsOfBuildInfo . buildInfo

toolsOfBuildInfo :: BuildInfo -> Set BuildTool
toolsOfBuildInfo =
  Set.fromList . fmap toolOfDependency . buildTools

#if MIN_VERSION_Cabal(2,0,0)
toolOfDependency :: LegacyExeDependency -> BuildTool
toolOfDependency = \case
  LegacyExeDependency name v ->
    let
      pname =
        mkPackageName $ T.pack name
    in
      BuildTool pname $ constraintsOfVersion pname v
#else
toolOfDependency :: Dependency -> BuildTool
toolOfDependency = \case
  Dependency name v ->
    let
      pname =
        mkPackageName $ T.pack (Cabal.unPackageName name)
    in
      BuildTool pname $ constraintsOfVersion pname v
#endif

constraintsOfVersion :: PackageName -> VersionRange -> [Constraint]
constraintsOfVersion name range =
  let
    go (lower, upper) =
      ConstraintBounded name (boundOfLower lower) (boundOfUpper upper)
  in
    if range == anyVersion then
      []
    else
      fmap go $ asVersionIntervals range

boundOfLower :: Cabal.LowerBound -> Bound
boundOfLower (Cabal.LowerBound ver b) =
  boundOfBound ver b

boundOfUpper :: Cabal.UpperBound -> Maybe Bound
boundOfUpper = \case
  Cabal.NoUpperBound ->
    Nothing
  Cabal.UpperBound ver b ->
    Just $ boundOfBound ver b

boundOfBound :: Cabal.Version -> Cabal.Bound -> Bound
boundOfBound ver = \case
  Cabal.InclusiveBound ->
    Inclusive ver
  Cabal.ExclusiveBound ->
    Exclusive ver

------------------------------------------------------------------------

data PackageType =
    Library
  | ExecutablesOnly
    deriving (Eq, Ord, Show)

getPackageType :: Directory -> EitherT CabalError IO PackageType
getPackageType dir = do
  withCabalFile dir $ \file -> do
    mt <- readPackageType file
    case mt of
      Nothing  ->
        left $ CabalCouldNotReadPackageType file
      Just t ->
        return t

readPackageType :: MonadIO m => File -> m (Maybe PackageType)
readPackageType cabalFile = do
  text <- liftM (fmap T.toLower . T.lines . fromMaybe T.empty) $ readUtf8 cabalFile
  if any (T.isPrefixOf "library") text then
    return (Just Library)
  -- Really, really old school .cabal files didn't have a library stanza,
  -- everything was at the top level, use 'exposed-modules' to detect this.
  else if any (T.isPrefixOf "exposed-modules") text then
    return (Just Library)
  else if any (T.isPrefixOf "executable") text then
    return (Just ExecutablesOnly)
  else
    return Nothing

------------------------------------------------------------------------

getPackageId :: Directory -> EitherT CabalError IO (Maybe PackageId)
getPackageId dir = do
  mf <- liftIO . runEitherT $ getCabalFile dir
  case mf of
    Left _ ->
      return Nothing
    Right f -> do
      liftM Just $ readPackageId f

readPackageId :: MonadIO m => File -> EitherT CabalError m PackageId
readPackageId file = do
  msrc <- liftM T.unpack `liftM` readUtf8 file
  case msrc of
    Nothing ->
      left $ CabalCouldNotReadPackageId file
    Just src ->
      case parsePackageDescription src of
        ParseFailed err ->
          left $ CabalFileParseError file err
        ParseOk _ gpd ->
          return $ takePackageId gpd

takePackageId :: GenericPackageDescription -> PackageId
takePackageId gpd =
  let
    Cabal.PackageIdentifier name version =
      Cabal.package $ Cabal.packageDescription gpd
  in
    PackageId (mkPackageName . T.pack $ Cabal.unPackageName name) version

------------------------------------------------------------------------

getSourcePackage :: Directory -> EitherT CabalError IO (Maybe SourcePackage)
getSourcePackage dir = do
  mpid <- getPackageId dir
  case mpid of
    Nothing  -> return Nothing -- no .cabal file found
    Just pid -> do
      hash <- hashSourcePackage dir
      return (Just (SourcePackage dir pid (sphHash hash)))

------------------------------------------------------------------------

data SourcePackageHash =
  SourcePackageHash {
      sphHash         :: Hash
    , sphFileListHash :: Hash
    , sphFileHashes   :: [(File, Hash)]
    } deriving (Eq, Ord, Show)

renderSourcePackageHash :: SourcePackageHash -> Text
renderSourcePackageHash (SourcePackageHash h lh fhs) =
  T.unlines $
    fmap (uncurry renderHashPart) fhs <>
    [ renderHashPart "(file list)" lh
    , renderHashPart "(package)" h ]

renderHashPart :: Text -> Hash -> Text
renderHashPart title hash =
  renderHash hash <> "  " <> title

fixupSDistDir :: MonadIO m => Directory -> m Directory
fixupSDistDir dir = do
  rdir <- tryMakeRelativeToCurrent dir
  if T.null rdir then
    return "."
  else
    return rdir

hashSourcePackage :: Directory -> EitherT CabalError IO SourcePackageHash
hashSourcePackage dir = do
  ignoredFiles <- Set.fromList <$> readIgnoredFiles dir

  rdir <- fixupSDistDir dir

  sdistFiles <- withSystemTempDirectory "mafia-sdist-" $ \tmp -> do
    let
      path = tmp </> "sources.txt"

    capture (CabalSDistFailed rdir) $
      callFrom (CabalSDistDisaster rdir) dir "cabal" ["sdist", "--list-sources=" <> path]

    mfile <- readUtf8 path
    case mfile of
      Nothing ->
        left (CabalSDistFailedCouldNotReadFile rdir path)
      Just utf8 ->
        return . Set.fromList . fmap normalise $ T.lines utf8

  let files = Set.toList (sdistFiles `Set.difference` ignoredFiles)

  hashes <- firstT CabalHashError $ mapM (hashFile . (dir </>)) files

  let fileListHash = hashText (T.unlines files)

  return SourcePackageHash {
      sphHash         = hashHashes (fileListHash : hashes)
    , sphFileListHash = fileListHash
    , sphFileHashes   = List.zip files hashes
    }

readIgnoredFiles :: Directory -> EitherT CabalError IO [File]
readIgnoredFiles dir = do
  mtxt <- readUtf8 (dir </> ".mafiaignore")
  return (maybe [] T.lines mtxt)
