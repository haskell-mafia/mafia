{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Mafia.Cabal.Version
  ( getCabalVersion
  , checkCabalVersion
  ) where

import           Control.Monad.Trans.Either (EitherT, left, runEitherT)

import qualified Data.Text as T

import           Mafia.Cabal.Process
import           Mafia.Cabal.Types
import           Mafia.Package
import           Mafia.Process

import           Mafia.P

import           System.IO (IO)

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
  --
  -- We need at least cabal-install-2.4.0.0 for v1- prefixes:
  --
  -- 2.4.0.0 Mikhail Glushenkov <mikhail.glushenkov@gmail.com> September 2018
  --   * Add 'v1-' prefixes for the commands that will be replaced in the
  --     new-build universe, in preparation for it becoming the default.  (#5358)
  --
  let vmin = makeVersion [2, 4]
      vmax = makeVersion [2, 5]

  version <- getCabalVersion

  case version >= vmin && version < vmax of
    False -> left (CabalInvalidVersion version vmin vmax)
    True  -> return ()
