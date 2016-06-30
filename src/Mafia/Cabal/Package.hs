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

import           Distribution.Package (Dependency(..))
import qualified Distribution.Package as Cabal (PackageName(..))
import           Distribution.PackageDescription (BuildInfo(..))
import           Distribution.PackageDescription (CondTree(..))
import           Distribution.PackageDescription (Executable(..))
import           Distribution.PackageDescription (GenericPackageDescription(..))
import           Distribution.PackageDescription (libBuildInfo)
import qualified Distribution.PackageDescription as Cabal (Library(..))
import           Distribution.PackageDescription.Parse (ParseResult(..))
import           Distribution.PackageDescription.Parse (parsePackageDescription)
import           Distribution.Version (VersionRange, anyVersion, asVersionIntervals)
import qualified Distribution.Version as Cabal (LowerBound(..), UpperBound(..), Bound(..))

import           Mafia.Cabal.Constraint
import           Mafia.Cabal.Types
import           Mafia.Hash
import           Mafia.IO
import           Mafia.Package
import           Mafia.Path
import           Mafia.Process

import           P

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT, left)

------------------------------------------------------------------------

getCabalFile :: MonadIO m => Directory -> m (Maybe File)
getCabalFile dir = do
  xs <- filter isCabalFile `liftM` getDirectoryContents dir
  case xs of
    []     -> return Nothing
    (x:[]) -> return (Just (dir </> x))
    (_:_)  -> return Nothing
  where
    isCabalFile fp =
      (takeExtension fp) == ".cabal" && not (T.isPrefixOf "." fp)

withCabalFile :: Directory -> (File -> EitherT CabalError IO a) -> EitherT CabalError IO a
withCabalFile dir io = do
  mf <- getCabalFile dir
  case mf of
    Nothing ->
      left $ CabalFileNotFound dir
    Just file ->
      io file

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
    loop (_, x, my) =
      toolsOfCondTree f x <>
      maybe Set.empty (toolsOfCondTree f) my
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

toolOfDependency :: Dependency -> BuildTool
toolOfDependency = \case
  Dependency (Cabal.PackageName name) v ->
    let
      pname =
        mkPackageName $ T.pack name
    in
      BuildTool pname $ constraintsOfVersion pname v

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

boundOfBound :: Version -> Cabal.Bound -> Bound
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
  mf <- getCabalFile dir
  case mf of
    Nothing -> return Nothing
    Just f  -> do
      mpid <- readPackageId f
      case mpid of
        Nothing  -> left (CabalCouldNotReadPackageId f)
        Just pid -> return (Just pid)

readPackageId :: MonadIO m => File -> m (Maybe PackageId)
readPackageId cabalFile = do
  text <- fromMaybe T.empty `liftM` readUtf8 cabalFile

  let findName    = fmap mkPackageName . findField "name"
      findVersion = (parseVersion =<<) . findField "version"

  let lines   = fmap T.words (T.lines text)
      name    = listToMaybe . mapMaybe findName    $ lines
      version = listToMaybe . mapMaybe findVersion $ lines

  return (PackageId <$> name <*> version)

findField :: Text -> [Text] -> Maybe Text
findField field = \case
  (key:value:_)
    | T.toLower (field <> ":") == T.toLower key ->
    Just value
  (_:_) ->
    Nothing
  [] ->
    Nothing

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
      sdistError _ = CabalSDistFailed rdir
      path = tmp </> "sources.txt"
    OutErr (_ :: Text) _ <- callFrom sdistError dir "cabal" ["sdist", "--list-sources=" <> path]
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
