{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Mafia.Makefile
  ( buildMakefile
  ) where

import qualified Data.Map as Map

import           Mafia.Cabal
import           Mafia.IO
import           Mafia.Path
import           Mafia.Process
import           Mafia.Error

import           Mafia.P

import           System.IO (IO)

import           Control.Monad.Trans.Bifunctor
import           Control.Monad.Trans.Either (EitherT)

buildMakefile :: Directory -> EitherT MafiaError IO ()
buildMakefile directory = do
  cabalFile <- firstT MafiaCabalError $ getCabalFile directory
  let makeFile = dropExtension cabalFile <> ".mk"
  whenM (doesFileExist makeFile) $ do
    callMakefile makeFile

callMakefile :: File -> EitherT MafiaError IO ()
callMakefile makeFile = do
  mafia <- getExecutablePath
  env   <- getEnvironment
  Pass  <- firstT MafiaProcessError
         $ callProcess
         $ Process
         { processCommand     = "make"
         , processArguments   = ["-f", makeFile]
         , processDirectory   = Nothing
         , processEnvironment = Just (Map.insert "MAFIA" mafia env) }
  return ()
