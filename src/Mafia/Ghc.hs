{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Mafia.Ghc
  ( GhcVersion
  , getGhcVersion

  , GhcError(..)
  , renderGhcError
  ) where

import           Data.Text (Text)
import qualified Data.Text as T

import           Mafia.Process

import           P

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT, left, runEitherT)


type GhcVersion = Text

data GhcError =
    GhcProcessError ProcessError
  | GhcNotInstalled
  deriving (Show)

renderGhcError :: GhcError -> Text
renderGhcError = \case
  GhcProcessError e ->
    renderProcessError e

  GhcNotInstalled ->
    mconcat
      [ "ghc is not installed."
      , "\nTo install:"
      , "\n - download from https://www.haskell.org/ghc/"
      , "\n - ./configure --prefix=$HOME/haskell/ghc-$VERSION  # or wherever you like to keep ghc"
      , "\n - make install"
      , "\n - ln -s $HOME/haskell/ghc-$VERSION $HOME/haskell/ghc"
      , "\n - add $HOME/haskell/ghc/bin to your $PATH" ]

getGhcVersion :: EitherT GhcError IO Text
getGhcVersion = do
  result <- runEitherT (call GhcProcessError "ghc" ["--version"])
  case result of
    Left  _         -> left GhcNotInstalled
    Right (Out out) ->
      case reverse (T.words out) of
        []      -> left GhcNotInstalled
        (ver:_) -> return ver
