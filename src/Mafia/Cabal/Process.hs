{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Mafia.Cabal.Process
  ( cabal
  , cabal_
  , cabalAnnihilate
  , cabalFrom
  ) where

import           Control.Monad.IO.Class (liftIO)

import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T

import           Mafia.Cabal.Types
import           Mafia.Process
import           Mafia.IO (getEnvironment)

import           Mafia.P

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT)
import           Control.Monad.Trans.Bifunctor (firstT)

cabal :: ProcessResult a => Argument -> [Argument] -> EitherT CabalError IO a
cabal cmd args = call CabalProcessError "cabal" (cmd : args)

cabal_ :: Argument -> [Argument] -> EitherT CabalError IO ()
cabal_ cmd args = do
  PassErr <- cabal cmd args
  return ()

cabalAnnihilate :: Argument -> [Argument] -> EitherT CabalError IO ()
cabalAnnihilate cmd args = do
  PassErrAnnihilate <- cabal cmd args
  return ()


cabalFrom ::
  ProcessResult a =>
  Directory ->
  SandboxConfigFile ->
  [Directory] ->
  Argument ->
  [Argument] ->
  EitherT CabalError IO a
cabalFrom dir sbcfg extraPath cmd args = do
  env <- liftIO (mkEnv sbcfg extraPath)

  let process =
        Process {
            processCommand     = "cabal"
          , processArguments   = cmd : args
          , processDirectory   = Just dir
          , processEnvironment = Just env
          }

  firstT CabalProcessError (callProcess process)

mkEnv :: SandboxConfigFile -> [Directory] -> IO (Map EnvKey EnvValue)
mkEnv sbcfg extraPaths =
  fmap (Map.insert "CABAL_SANDBOX_CONFIG" sbcfg . prependPaths extraPaths) getEnvironment

prependPaths :: [Directory] -> Map EnvKey EnvValue -> Map EnvKey EnvValue
prependPaths new kvs =
  let
    key =
      "PATH"
  in
    case Map.lookup key kvs of
      Nothing ->
        Map.insert key (T.intercalate ":" new) kvs
      Just old ->
        Map.insert key (T.intercalate ":" $ new <> T.splitOn ":" old) kvs
