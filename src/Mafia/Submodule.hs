{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Mafia.Submodule
  ( SubmoduleError(..)
  , renderSubmoduleError
  , syncCabalSources
  , getSandboxSources
  , getSubmoduleSources
  ) where

import           Control.Monad.IO.Class (MonadIO(..))

import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Mafia.Cabal
import           Mafia.Git
import           Mafia.IO
import           Mafia.Path
import           Mafia.Process
import           Mafia.Project

import           P

import           System.IO (IO, stderr)

import           X.Control.Monad.Trans.Either (EitherT)


data SubmoduleError =
    SubmoduleProjectError ProjectError
  | SubmoduleCabalError CabalError
  | SubmoduleGitError GitError
  deriving (Show)

renderSubmoduleError :: SubmoduleError -> Text
renderSubmoduleError = \case
  SubmoduleProjectError e ->
    renderProjectError e

  SubmoduleCabalError e ->
    renderCabalError e

  SubmoduleGitError e ->
    renderGitError e

syncCabalSources :: EitherT SubmoduleError IO ()
syncCabalSources = do
  repairSandbox
  installed <- getSandboxSources
  required  <- getSubmoduleSources
  traverse_ addSandboxSource    (required `Set.difference` installed)
  traverse_ removeSandboxSource (installed `Set.difference` required)

repairSandbox :: EitherT SubmoduleError IO ()
repairSandbox = do
  dir <- firstT SubmoduleCabalError initSandbox
  firstT SubmoduleCabalError (repairIndexFile dir)

addSandboxSource :: Directory -> EitherT SubmoduleError IO ()
addSandboxSource dir = do
  rel <- fromMaybe dir <$> makeRelativeToCurrentDirectory dir
  liftIO (T.hPutStrLn stderr ("Sandbox: Adding " <> rel))
  firstT SubmoduleCabalError $ sandbox_ "add-source" [dir]

removeSandboxSource :: Directory -> EitherT SubmoduleError IO ()
removeSandboxSource dir = do
  rel <- fromMaybe dir <$> makeRelativeToCurrentDirectory dir
  liftIO (T.hPutStrLn stderr ("Sandbox: Removing " <> rel))
  firstT SubmoduleCabalError $ sandbox_ "delete-source" ["-v0", dir]

getSandboxSources :: EitherT SubmoduleError IO (Set Directory)
getSandboxSources = do
  Out sources <- firstT SubmoduleCabalError $ sandbox "list-sources" []

  let dropHeader = drop 3
      dropFooter = reverse . drop 2 . reverse

  return . Set.fromList
         . dropFooter
         . dropHeader
         . T.lines
         $ sources

getSubmoduleSources :: EitherT SubmoduleError IO (Set Directory)
getSubmoduleSources = Set.union <$> getConfiguredSources
                                <*> firstT SubmoduleGitError getConventionSources

getConfiguredSources :: EitherT SubmoduleError IO (Set Directory)
getConfiguredSources = do
  dir <- getCurrentDirectory
  root <- firstT SubmoduleGitError getProjectRoot
  name <- firstT SubmoduleProjectError $ getProjectName dir
  cfg <- readUtf8 (name <> ".submodules")
  return . Set.fromList
         . fmap (root </>)
         . T.lines
         . fromMaybe T.empty
         $ cfg

getConventionSources :: EitherT GitError IO (Set Directory)
getConventionSources = do
  -- make sure we never include the current directory (i.e. the project we're
  -- trying to build) as a source dependency
  dir <- getCurrentDirectory
  root <- getProjectRoot
  repoPaths <- Set.filter (/= dir) <$> getSourcesFrom root

  subs <- fmap ((root </>) . subName) <$> getSubmodules
  subPaths <- Set.unions <$> traverse getSourcesFrom subs

  return $ Set.union repoPaths subPaths

getSourcesFrom :: MonadIO m => Directory -> m (Set Path)
getSourcesFrom dir = do
  let cleanDir x = dropTrailingPathSeparator `liftM` canonicalizePath x

  dir'    <- cleanDir dir
  entries <- getDirectoryListing (RecursiveDepth 3) dir'
  sources <- mapM cleanDir
           . fmap   (dir' </>)
           . fmap   (takeDirectory)
           . filter (not . T.isInfixOf ".cabal-sandbox/")
           . filter (not . T.isPrefixOf "lib/")
           . filter (not . T.isPrefixOf "bin/")
           . filter (extension ".cabal")
           . fmap   (T.drop (T.length dir' + 1))
           $ entries

  return (Set.fromList sources)
