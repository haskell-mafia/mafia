{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Mafia.Error
  ( MafiaError (..)
  , CacheUpdate (..)
  , renderMafiaError
  , liftGit
  ) where

import           Control.Exception (IOException)

import           Data.Text (Text)
import qualified Data.Text as T

import           Mafia.Cabal.Types
import           Mafia.Git
import           Mafia.Process
import           Mafia.Project

import           P

import           X.Control.Monad.Trans.Either (EitherT, firstEitherT)


-- FIX This should live in Cache
data CacheUpdate
  = Add    File File
  | Update File File
  | Delete File
  deriving (Eq, Ord, Show)

-- FIX Leaving this to make code cleanup easier, but ideally is a union of
-- sub-exceptions rather than this module being the root of most dependencies
data MafiaError
  = MafiaProjectError ProjectError
  | MafiaCabalError CabalError
  | MafiaProcessError ProcessError
  | MafiaParseError Text
  | MafiaCacheUpdateError CacheUpdate IOException
  | MafiaEntryPointNotFound File
  deriving (Show)


renderMafiaError :: MafiaError -> Text
renderMafiaError = \case
  MafiaProjectError e
   -> renderProjectError e

  MafiaCabalError e
   -> renderCabalError e

  MafiaProcessError e
   -> renderProcessError e

  MafiaParseError msg
   -> "Parse failed: " <> msg

  MafiaCacheUpdateError x ex
   -> "Cache update failed: " <> T.pack (show x)
   <> "\n" <> T.pack (show ex)

  MafiaEntryPointNotFound path
   -> "GHCi entry point not found: " <> path

liftGit :: Functor m => EitherT GitError m a -> EitherT MafiaError m a
liftGit = firstEitherT $ \case
  GitParseError   msg -> MafiaParseError   msg
  GitProcessError err -> MafiaProcessError err
