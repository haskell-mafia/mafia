{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Mafia.Cabal.Package
  ( getPackageId
  , getCabalFile

  , SourcePackage(..)
  , getSourcePackage

  , SourcePackageHash(..)
  , renderSourcePackageHash
  , hashSourcePackage
  ) where

import           Control.Monad.IO.Class (MonadIO(..))

import qualified Data.List as List
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T

import           Mafia.Cabal.Types
import           Mafia.Hash
import           Mafia.IO
import           Mafia.Package
import           Mafia.Path
import           Mafia.Process

import           P

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT, firstEitherT, left)

------------------------------------------------------------------------

getCabalFile :: MonadIO m => Directory -> m (Maybe File)
getCabalFile dir = do
  xs <- filter ((== ".cabal") . takeExtension) `liftM` getDirectoryContents dir
  case xs of
    []     -> return Nothing
    (x:[]) -> return (Just (dir </> x))
    (_:_)  -> return Nothing

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

  let findName    = fmap PackageName   . findField "name"
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

hashSourcePackage :: Directory -> EitherT CabalError IO SourcePackageHash
hashSourcePackage dir = do
  ignoredFiles <- Set.fromList <$> readIgnoredFiles dir

  rdir <- tryMakeRelativeToCurrent dir
  let sdistError _ = CabalSDistFailed rdir
  OutErr _ err <- callFrom sdistError dir "cabal" ["sdist", "--list-sources=/dev/stderr"]

  let sdistFiles = Set.fromList (fmap normalise (T.lines err))
      files      = Set.toList (sdistFiles `Set.difference` ignoredFiles)

  hashes <- firstEitherT CabalHashError $ mapM (hashFile . (dir </>)) files

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
