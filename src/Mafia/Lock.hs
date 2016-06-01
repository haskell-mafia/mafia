{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Mafia.Lock (
    LockError(..)
  , renderLockError

  , getLockFile
  , readLockFile
  , writeLockFile
  ) where

import qualified Data.Text as T

import           Mafia.Cabal
import           Mafia.Ghc
import           Mafia.IO
import           Mafia.Path
import           Mafia.Project

import           P

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT, hoistEither, left)


data LockError =
    LockProjectError ProjectError
  | LockCabalError CabalError
  | LockGhcError GhcError
  | LockFileVersionUnknown File Text
    deriving (Show)

renderLockError :: LockError -> Text
renderLockError = \case
  LockProjectError e ->
    renderProjectError e
  LockCabalError e ->
    renderCabalError e
  LockGhcError e ->
    renderGhcError e
  LockFileVersionUnknown file header ->
    "Cannot read lock file: " <> file <>
    "\nExpected Header> " <> lockFileHeader <>
    "\n  Actual Header> " <> header

getLockFile :: Directory -> EitherT LockError IO File
getLockFile dir = do
  ghcver <- firstT LockGhcError getGhcVersion
  project <- firstT LockProjectError $ getProjectName dir
  return $ dir </> project <> ".lock-" <> ghcver

lockFileHeader :: Text
lockFileHeader =
  "# mafia-lock-file-version: 0"

readLockFile :: File -> EitherT LockError IO (Maybe [Constraint])
readLockFile file = do
  mtxt <- readUtf8 file
  case mtxt of
    Nothing ->
      pure Nothing
    Just txt ->
      case T.lines txt of
        (x : xs) | x == lockFileHeader ->
          fmap Just $
          traverse (hoistEither . first LockCabalError . parseConstraint) xs
        (x : _) ->
          left $ LockFileVersionUnknown file x
        [] ->
          left $ LockFileVersionUnknown file ""

writeLockFile :: File -> [Constraint] -> EitherT LockError IO ()
writeLockFile file xs = do
  writeUtf8 file . T.unlines $
    lockFileHeader :
    fmap renderConstraint xs
