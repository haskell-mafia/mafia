{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module Mafia.Project
  ( ProjectName
  , getProjectName

  , ProjectError (..)
  , renderProjectError
  ) where

import           Data.Text (Text)
import qualified Data.Text as T

import           Mafia.IO
import           Mafia.Path

import           P

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT, pattern EitherT)


type ProjectName = Text

data ProjectError
  = ProjectNotFound
  | MultipleProjectsFound [ProjectName]
  deriving (Show)


renderProjectError :: ProjectError -> Text
renderProjectError = \case
  ProjectNotFound ->
    "Could not find .cabal project"

  MultipleProjectsFound ps ->
    "Found multiple possible .cabal projects: " <> T.intercalate ", " ps


getProjectName :: EitherT ProjectError IO ProjectName
getProjectName = EitherT $ do
  entries <- filterM doesFileExist =<< getDirectoryContents "."

  let projects = fmap dropExtension
               . filter ((== ".cabal") . takeExtension)
               $ entries

  case projects of
    []  -> return (Left ProjectNotFound)
    [p] -> return (Right p)
    ps  -> return (Left (MultipleProjectsFound ps))
