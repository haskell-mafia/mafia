{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Mafia.Ghc (
    GhcVersion(..)
  , mkGhcVersion
  , renderGhcVersion
  , getGhcVersion

  , GhcTarget(..)
  , getGhcTarget

  , GhcError(..)
  , renderGhcError
  ) where

import qualified Data.Text as T

import           Mafia.Package
import           Mafia.Process

import           P

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT, left, runEitherT)


newtype GhcVersion =
  GhcVersion {
      unGhcVersion :: Version
    } deriving (Eq, Ord, Show)

newtype GhcTarget =
  GhcTarget {
      unGhcTarget :: Text
    } deriving (Eq, Ord, Show)

data GhcError =
    GhcProcessError !ProcessError
  | GhcCannotParseVersion !Text
  | GhcNotInstalled
    deriving (Show)

renderGhcError :: GhcError -> Text
renderGhcError = \case
  GhcProcessError e ->
    renderProcessError e

  GhcCannotParseVersion v ->
    "ghc returned an invalid version: " <> v

  GhcNotInstalled ->
    mconcat
      [ "ghc is not installed."
      , "\nTo install:"
      , "\n - download from https://www.haskell.org/ghc/"
      , "\n - ./configure --prefix=$HOME/haskell/ghc-$VERSION  # or wherever you like to keep ghc"
      , "\n - make install"
      , "\n - ln -s $HOME/haskell/ghc-$VERSION $HOME/haskell/ghc"
      , "\n - add $HOME/haskell/ghc/bin to your $PATH" ]

mkGhcVersion :: [Int] -> GhcVersion
mkGhcVersion vs =
  GhcVersion $ makeVersion vs

renderGhcVersion :: GhcVersion -> Text
renderGhcVersion =
  renderVersion . unGhcVersion

getGhcVersion :: EitherT GhcError IO GhcVersion
getGhcVersion = do
  v0 <- ghc "--numeric-version"
  case parseVersion v0 of
    Nothing ->
      left $ GhcCannotParseVersion v0
    Just v ->
      pure $ GhcVersion v

getGhcTarget :: EitherT GhcError IO GhcTarget
getGhcTarget = do
  GhcTarget <$> ghc "--print-target-platform"

ghc :: Text -> EitherT GhcError IO Text
ghc argument = do
  result <- runEitherT (call GhcProcessError "ghc" [argument])
  case result of
    Left _ ->
      left GhcNotInstalled
    Right (Out out) ->
      pure $ T.strip out
