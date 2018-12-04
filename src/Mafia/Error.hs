{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Mafia.Error
  ( MafiaError (..)
  , renderMafiaError
  , liftCabal
  ) where

import           Control.Monad.Trans.Either (EitherT)
import           Control.Monad.Trans.Bifunctor (firstT)

import           Mafia.Bin
import           Mafia.Cabal.Types
import           Mafia.Cache
import           Mafia.Ghc
import           Mafia.Git
import           Mafia.Hash
import           Mafia.Init
import           Mafia.Install
import           Mafia.Lock
import           Mafia.P
import           Mafia.Process
import           Mafia.Script
import           Mafia.Submodule

-- FIX Leaving this to make code cleanup easier, but ideally is a union of
-- sub-exceptions rather than this module being the root of most dependencies
data MafiaError =
    MafiaProcessError ProcessError
  | MafiaGitError GitError
  | MafiaGhcError GhcError
  | MafiaCacheError CacheError
  | MafiaCabalError CabalError
  | MafiaSubmoduleError SubmoduleError
  | MafiaInstallError InstallError
  | MafiaBinError BinError
  | MafiaHashError HashError
  | MafiaInitError InitError
  | MafiaLockError LockError
  | MafiaScriptError ScriptError
  | MafiaNoInstallConstraints
  | MafiaNoPackageKeys
  | MafiaParseError Text
  | MafiaEntryPointNotFound File
  deriving (Show)

renderMafiaError :: MafiaError -> Text
renderMafiaError = \case
  MafiaProcessError e ->
    renderProcessError e

  MafiaGitError e ->
    renderGitError e

  MafiaGhcError e ->
    renderGhcError e

  MafiaCacheError e ->
    renderCacheError e

  MafiaCabalError e ->
    renderCabalError e

  MafiaSubmoduleError e ->
    renderSubmoduleError e

  MafiaInstallError e ->
    renderInstallError e

  MafiaBinError e ->
    renderBinError e

  MafiaHashError e ->
    renderHashError e

  MafiaInitError e ->
    renderInitError e

  MafiaLockError e ->
    renderLockError e

  MafiaScriptError e ->
    renderScriptError e

  MafiaNoInstallConstraints ->
    "Could not find the dependency constraints calculated during the last install."

  MafiaNoPackageKeys ->
    "Could not find the keys of the packages installed during the last build."

  MafiaParseError msg ->
    "Parse failed: " <> msg

  MafiaEntryPointNotFound path ->
    "GHCi entry point not found: " <> path

liftCabal :: Functor m => EitherT CabalError m a -> EitherT MafiaError m a
liftCabal =
  firstT MafiaCabalError
