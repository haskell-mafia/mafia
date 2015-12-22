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
  | CabalGhcNotInstalled
  deriving (Show)


renderCabalError :: CabalError -> Text
renderCabalError = \case
  CabalProcessError e
   -> renderProcessError e

  CabalProjectError e
   -> renderProjectError e

  CabalIndexFileNotFound file
   -> "Index file not found: " <> file

  CabalCorruptIndexFile tarError
   -> "Corrupt index file: " <> T.pack (show tarError)

  CabalGhcNotInstalled
   -> "ghc is not installed."
   <> "\nTo install:"
   <> "\n - download from https://www.haskell.org/ghc/"
   <> "\n - ./configure --prefix=$HOME/haskell/ghc-$VERSION  # or wherever you like to keep ghc"
   <> "\n - make install"
   <> "\n - ln -s $HOME/haskell/ghc-$VERSION $HOME/haskell/ghc"
   <> "\n - add $HOME/haskell/ghc/bin to your $PATH"
