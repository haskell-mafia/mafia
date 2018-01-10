{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
module Mafia.Include
  ( getIncludeDirs
  ) where

import qualified Data.Text as T

import           Mafia.Cabal
import           Mafia.IO
import           Mafia.Path
import           Mafia.Error

import           Mafia.P

import           System.IO (IO)

import           Control.Monad.Trans.Bifunctor
import           Control.Monad.Trans.Either (EitherT)

getIncludeDirs :: EitherT MafiaError IO [Path]
getIncludeDirs = do
  packageDB     <- firstT MafiaCabalError getPackageDB
  subdirs       <- getDirectoryListing (RecursiveDepth 0) packageDB
  let packages = filter (extension ".conf") subdirs
  concat <$> mapM readIncludeDirs packages

readIncludeDirs :: File -> EitherT MafiaError IO [Path]
readIncludeDirs package = do
  contents <- readUtf8 package
  return $
    case contents of
     Nothing -> []
     Just txt -> concatMap parseLine $ T.lines txt

parseLine :: Text -> [Path]
parseLine line
 | (i:is) <- T.words line
 , T.toLower i == "include-dirs:"
 = is
 | otherwise
 = []
