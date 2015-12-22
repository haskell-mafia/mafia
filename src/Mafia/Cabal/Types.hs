{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Mafia.Cabal.Types
  ( SandboxDir
  , CabalError(..)
  , renderCabalError
  ) where

import qualified Codec.Archive.Tar as Tar

import           Data.Text (Text)
import qualified Data.Text as T

import           Mafia.Ghc
import           Mafia.Path
import           Mafia.Process
import           Mafia.Project

import           P


type SandboxDir = Directory

data CabalError =
    CabalProcessError ProcessError
  | CabalProjectError ProjectError
  | CabalIndexFileNotFound File
  | CabalCorruptIndexFile Tar.FormatError
  | CabalGhcError GhcError
  deriving (Show)


renderCabalError :: CabalError -> Text
renderCabalError = \case
  CabalProcessError e ->
    renderProcessError e

  CabalProjectError e ->
    renderProjectError e

  CabalIndexFileNotFound file ->
    "Index file not found: " <> file

  CabalCorruptIndexFile tarError ->
    "Corrupt index file: " <> T.pack (show tarError)

  CabalGhcError e ->
    renderGhcError e
