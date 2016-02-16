{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Mafia.Cabal.Process
  ( cabal
  , cabal_
  , cabalFrom
  ) where

import           Control.Monad.IO.Class (liftIO)

import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T

import           Mafia.Cabal.Types
import           Mafia.Process

import           P

import           System.Environment (getEnvironment)
import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT)


cabal :: ProcessResult a => Argument -> [Argument] -> EitherT CabalError IO a
cabal cmd args = call CabalProcessError "cabal" (cmd : args)

cabal_ :: Argument -> [Argument] -> EitherT CabalError IO ()
cabal_ cmd args = do
  PassErr <- cabal cmd args
  return ()

cabalFrom
  :: ProcessResult a
  => Directory
  -> Maybe SandboxConfigFile
  -> Argument
  -> [Argument]
  -> EitherT CabalError IO a
cabalFrom dir sbcfg cmd args = do
  menv <- liftIO (mkEnv sbcfg)

  let process =
        Process {
            processCommand     = "cabal"
          , processArguments   = cmd : args
          , processDirectory   = Just dir
          , processEnvironment = menv
          }

  firstT CabalProcessError (callProcess process)

mkEnv :: Maybe SandboxConfigFile -> IO (Maybe (Map EnvKey EnvValue))
mkEnv sbcfg =
  case sbcfg of
    Nothing  -> return Nothing
    Just cfg -> Just <$> mergeEnv (Map.fromList [("CABAL_SANDBOX_CONFIG", cfg)])

mergeEnv :: Map EnvKey EnvValue -> IO (Map EnvKey EnvValue)
mergeEnv extra = do
  env <- Map.fromList . fmap (bimap T.pack T.pack) <$> getEnvironment
  return (Map.union extra env)
