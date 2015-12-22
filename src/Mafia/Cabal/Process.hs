{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Mafia.Cabal.Process
  ( cabal
  , cabal_
  ) where

import           Mafia.Error
import           Mafia.Process

import           P

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT)


cabal :: ProcessResult a => Argument -> [Argument] -> EitherT MafiaViolation IO a
cabal cmd args = call ProcessError "cabal" (cmd : args)

cabal_ :: Argument -> [Argument] -> EitherT MafiaViolation IO ()
cabal_ cmd args = do
  Pass <- cabal cmd args
  return ()
