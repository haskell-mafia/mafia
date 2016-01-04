{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Mafia.Cabal.Package
  ( getPackageId
  , getCabalFile

  , SourcePackage(..)
  , getSourcePackage
  , hashSDist
  ) where

import           Control.Monad.IO.Class (MonadIO(..))

import qualified Data.List as List
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
      hash <- hashSDist dir
      return (Just (SourcePackage dir pid hash))

hashSDist :: Directory -> EitherT CabalError IO Hash
hashSDist dir = do
  OutErr _ err <- callFrom CabalProcessError dir "cabal" ["sdist", "--list-sources=/dev/stderr"]
  let files = List.sort (fmap normalise (T.lines err))
  hashes <- firstEitherT CabalHashError $ mapM (hashFile . (dir </>)) files
  return $ hashHashes (hashText (T.unlines files) : hashes)
