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

import           P

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT)

buildMakefile :: EitherT MafiaError IO ()
buildMakefile = do
  cabalFile <- firstT MafiaCabalError $ getCabalFile =<< getCurrentDirectory
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

