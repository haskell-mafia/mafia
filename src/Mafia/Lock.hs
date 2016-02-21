{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Mafia.Lock (
    LockError(..)
  , renderLockError

  , getLockFile
  , readConstraints
  , writeConstraints
  ) where

import           Data.Text (Text)
import qualified Data.Text as T

import           Mafia.Cabal
import           Mafia.Ghc
import           Mafia.IO
import           Mafia.Path
import           Mafia.Project

import           P

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT, hoistEither)


data LockError =
    LockProjectError ProjectError
  | LockCabalError CabalError
  | LockGhcError GhcError
    deriving (Show)

renderLockError :: LockError -> Text
renderLockError = \case
  LockProjectError e ->
    renderProjectError e
  LockCabalError e ->
    renderCabalError e
  LockGhcError e ->
    renderGhcError e

getLockFile :: Directory -> EitherT LockError IO File
getLockFile dir = do
  ghcver <- firstT LockGhcError getGhcVersion
  project <- firstT LockProjectError $ getProjectName dir
  return $ dir </> project <> ".lock-" <> ghcver

readConstraints :: File -> EitherT LockError IO (Maybe [Constraint])
readConstraints file = do
  mtxt <- readUtf8 file
  case mtxt of
    Nothing ->
      pure Nothing
    Just txt ->
      fmap Just .
      traverse (hoistEither . first LockCabalError . parseConstraint) $
      T.lines txt

writeConstraints :: File -> [Constraint] -> EitherT LockError IO ()
writeConstraints file xs = do
  createDirectoryIfMissing True (takeDirectory file)
  writeUtf8 file . T.unlines $ fmap renderConstraint xs
