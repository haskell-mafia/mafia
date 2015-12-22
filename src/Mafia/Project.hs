{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module Mafia.Project
  ( ProjectError (..)
  , ProjectName
  , getProjectName
  ) where

import           Data.Text (Text)

import           Mafia.IO
import           Mafia.Path

import           P

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT, pattern EitherT)


data ProjectError
  = ProjectNotFound
  | MultipleProjectsFound [ProjectName]
  deriving (Show)

type ProjectName = Text


getProjectName :: EitherT ProjectError IO ProjectName
getProjectName = EitherT $ do
  entries <- getDirectoryContents "."

  let projects = fmap dropExtension
               . filter ((== ".cabal") . takeExtension)
               $ entries

  case projects of
    []  -> return (Left ProjectNotFound)
    [p] -> return (Right p)
    ps  -> return (Left (MultipleProjectsFound ps))
