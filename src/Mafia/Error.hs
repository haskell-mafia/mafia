{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Mafia.Error
  ( MafiaViolation (..)
  , CacheUpdate (..)
  , renderViolation
  , liftGit
  ) where

import           Control.Exception (IOException)

import           Data.Text (Text)
import qualified Data.Text as T

import           Mafia.Cabal
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

-- FIX Leaving this to make code cleanup easier, but ideally is a union of sub-exceptions rather than this module being
-- the root of most dependencies
data MafiaViolation
  = MafiaProjectError ProjectError
  | CabalError CabalError
  | ProcessError ProcessError
  | ParseError Text
  | CacheUpdateError CacheUpdate IOException
  | EntryPointNotFound File
  | GhcNotInstalled
  deriving (Show)


renderViolation :: MafiaViolation -> Text
renderViolation = \case
  MafiaProjectError ProjectNotFound
   -> "Could not find .cabal project"

  MafiaProjectError (MultipleProjectsFound ps)
   -> "Found multiple possible .cabal projects: "
   <> T.intercalate ", " ps

  CabalError (IndexFileNotFound file)
   -> "Index file not found: " <> file

  CabalError (CorruptIndexFile tarError)
   -> "Corrupt index file: " <> T.pack (show tarError)

  ProcessError (ProcessFailure p code)
   -> "Process failed: " <> T.intercalate " " (processCommand p : processArguments p)
   <> " (exit code: " <> T.pack (show code) <> ")"

  ProcessError (ProcessException p ex)
   -> "Process failed: " <> T.intercalate " " (processCommand p : processArguments p)
   <> "\n" <> T.pack (show ex)

  ParseError msg
   -> "Parse failed: " <> msg

  CacheUpdateError x ex
   -> "Cache update failed: " <> T.pack (show x)
   <> "\n" <> T.pack (show ex)

  EntryPointNotFound path
   -> "GHCi entry point not found: " <> path

  GhcNotInstalled
   -> "ghc is not installed."
   <> "\nTo install:"
   <> "\n - download from https://www.haskell.org/ghc/"
   <> "\n - ./configure --prefix=$HOME/haskell/ghc-$VERSION  # or wherever you like to keep ghc"
   <> "\n - make install"
   <> "\n - ln -s $HOME/haskell/ghc-$VERSION $HOME/haskell/ghc"
   <> "\n - add $HOME/haskell/ghc/bin to your $PATH"

liftGit :: Functor m => EitherT GitError m a -> EitherT MafiaViolation m a
liftGit = firstEitherT $ \case
  GitParseError   msg -> ParseError   msg
  GitProcessError err -> ProcessError err
