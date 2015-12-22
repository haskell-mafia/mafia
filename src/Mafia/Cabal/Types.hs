{-# LANGUAGE NoImplicitPrelude #-}
module Mafia.Cabal.Types
  ( SandboxDir
  , CabalError(..)
  ) where

import qualified Codec.Archive.Tar as Tar

import           Mafia.Path

import           P


type SandboxDir = Directory

data CabalError =
    IndexFileNotFound File
  | CorruptIndexFile Tar.FormatError
  deriving (Show)
