{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Mafia.Cabal.Process
  ( cabal
  , cabal_
  ) where

import           Mafia.Cabal.Types
import           Mafia.Process

import           P

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT)


cabal :: ProcessResult a => Argument -> [Argument] -> EitherT CabalError IO a
cabal cmd args = call CabalProcessError "cabal" (cmd : args)

cabal_ :: Argument -> [Argument] -> EitherT CabalError IO ()
cabal_ cmd args = do
  Pass <- cabal cmd args
  return ()
