{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Mafia.Cabal.Package
  ( getPackageId
  , getCabalFile
  , readPackageId

  , SourcePackage(..)
  , getSourcePackage
  , hashSDist
  ) where

import           Control.Monad.IO.Class (MonadIO(..))

import qualified Data.List as List
import qualified Data.Text as T

import           Mafia.Cabal.Types
import           Mafia.Hash
import           Mafia.IO
import           Mafia.Package
import           Mafia.Path
import           Mafia.Process

import           P

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT, firstEitherT)

------------------------------------------------------------------------

getPackageId :: MonadIO m => Directory -> m (Maybe PackageId)
getPackageId dir = do
  mf <- getCabalFile dir
  case mf of
    Nothing -> return Nothing
    Just f  -> readPackageId f

getCabalFile :: MonadIO m => Directory -> m (Maybe File)
getCabalFile dir = do
  xs <- filter ((== ".cabal") . takeExtension) `liftM` getDirectoryContents dir
  case xs of
    []     -> return Nothing
    (x:[]) -> return (Just (dir </> x))
    (_:_)  -> return Nothing

readPackageId :: MonadIO m => File -> m (Maybe PackageId)
readPackageId cabalFile = do
  text <- fromMaybe T.empty `liftM` readUtf8 cabalFile

  let findName ("name:":name:_) = Just (PackageName name)
      findName _                = Nothing

  let findVersion ("version:":version:_) = parseVersion version
      findVersion _                      = Nothing

  let lines   = fmap T.words (T.lines text)
      name    = listToMaybe . mapMaybe findName    $ lines
      version = listToMaybe . mapMaybe findVersion $ lines

  return (PackageId <$> name <*> version)

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
  let files = List.sort (T.lines err)
  hashes <- firstEitherT CabalHashError $ mapM (hashFile . (dir </>)) files
  return $ hashHashes (hashText (T.unlines files) : hashes)
