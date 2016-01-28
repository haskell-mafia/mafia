{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Mafia.Error
  ( MafiaError (..)
  , renderMafiaError
  , liftCabal
  ) where

import           Data.Text (Text)

import           Mafia.Cabal.Types
import           Mafia.Git
import           Mafia.Hash
import           Mafia.Init
import           Mafia.Install
import           Mafia.Process
import           Mafia.Project
import           Mafia.Submodule

import           P

import           X.Control.Monad.Trans.Either (EitherT)


-- FIX Leaving this to make code cleanup easier, but ideally is a union of
-- sub-exceptions rather than this module being the root of most dependencies
data MafiaError
  = MafiaProjectError ProjectError
  | MafiaProcessError ProcessError
  | MafiaGitError GitError
  | MafiaCabalError CabalError
  | MafiaSubmoduleError SubmoduleError
  | MafiaInstallError InstallError
  | MafiaHashError HashError
  | MafiaInitError InitError
  | MafiaParseError Text
  | MafiaEntryPointNotFound File
  deriving (Show)


renderMafiaError :: MafiaError -> Text
renderMafiaError = \case
  MafiaProjectError e ->
    renderProjectError e

  MafiaProcessError e ->
    renderProcessError e

  MafiaGitError e ->
    renderGitError e

  MafiaCabalError e ->
    renderCabalError e

  MafiaSubmoduleError e ->
    renderSubmoduleError e

  MafiaInstallError e ->
    renderInstallError e

  MafiaHashError e ->
    renderHashError e

  MafiaInitError e ->
    renderInitError e

  MafiaParseError msg ->
    "Parse failed: " <> msg

  MafiaEntryPointNotFound path ->
    "GHCi entry point not found: " <> path

liftCabal :: Functor m => EitherT CabalError m a -> EitherT MafiaError m a
liftCabal =
  firstT MafiaCabalError
