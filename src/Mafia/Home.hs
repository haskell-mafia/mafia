{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module Mafia.Home
  ( getMafiaHome
  , getMafiaDir
  , ensureMafiaDir
  ) where

import           Control.Monad.IO.Class (MonadIO(..))

import qualified Data.Text as T

import           Mafia.IO
import           Mafia.Path

import           P


getMafiaHome :: MonadIO m => m Directory
getMafiaHome = do
  mhome <- lookupEnv "MAFIA_HOME"
  bind canonicalizePath $
    case mhome of
      Just home ->
        return home
      Nothing ->
        (</> T.pack ".ambiata/mafia") `liftM` getHomeDirectory

getMafiaDir :: MonadIO m => Directory -> m Directory
getMafiaDir path = do
  home <- getMafiaHome
  return (home </> path)

ensureMafiaDir :: MonadIO m => Directory -> m Directory
ensureMafiaDir path = do
  path' <- getMafiaDir path
  createDirectoryIfMissing True path'
  return path'
