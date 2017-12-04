{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Mafia.Cabal.Version
  ( getCabalVersion
  , checkCabalVersion
  ) where

import           Control.Monad.IO.Class (liftIO)

import qualified Data.Text as T

import           Mafia.Cabal.Process
import           Mafia.Cabal.Types
import           Mafia.Package
import           Mafia.Process

import           P

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT, left, runEitherT)


getCabalVersion :: EitherT CabalError IO Version
getCabalVersion = do
  result <- liftIO . runEitherT $ cabal "--version" []
  case result of
    Left  _         -> left CabalNotInstalled
    Right (Out out) ->
      case parseCabalVersion out of
        Nothing  -> left (CabalCouldNotParseCabalVersion out)
        Just ver -> return ver

parseCabalVersion :: Text -> Maybe Version
parseCabalVersion out =
  case T.words out of
    ("cabal-install" : "version" : version : _) -> parseVersion version
    _                                           -> Nothing

-- TODO not sure if maybe we should have no upper bounds here?
-- TODO maybe mafia should just install its own cabal?
checkCabalVersion :: EitherT CabalError IO ()
checkCabalVersion = do
  -- We need at least cabal-install-1.22.4 for the "cabal install --dry-run -v2" in
  -- Mafia.Cabal.Dependencies to get the right output.
  --
  -- It's this commit that we need:
  --   https://github.com/haskell/cabal/commit/c0b3c7f1b6ae7bb7663a2c18578ede95d6a40919
  let vmin = makeVersion [1,22,4]
      vmax = makeVersion [2, 1]

  version <- getCabalVersion

  case version >= vmin && version < vmax of
    False -> left (CabalInvalidVersion version vmin vmax)
    True  -> return ()
