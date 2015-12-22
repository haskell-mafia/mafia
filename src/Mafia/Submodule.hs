{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Mafia.Submodule
  ( syncCabalSources
  , getSandboxSources
  , getSubmoduleSources
  ) where

import           Control.Monad.IO.Class (MonadIO(..))

import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Mafia.Cabal
import           Mafia.Error
import           Mafia.Git
import           Mafia.IO
import           Mafia.Path
import           Mafia.Process
import           Mafia.Project

import           P

import           System.IO (IO, stderr)

import           X.Control.Monad.Trans.Either (EitherT, firstEitherT)


syncCabalSources :: EitherT MafiaViolation IO ()
syncCabalSources = do
  repairSandbox
  installed <- getSandboxSources
  required  <- getSubmoduleSources
  traverse_ addSandboxSource    (required `Set.difference` installed)
  traverse_ removeSandboxSource (installed `Set.difference` required)

repairSandbox :: EitherT MafiaViolation IO ()
repairSandbox = do
  sandboxDir <- initSandbox
  firstEitherT CabalError (repairIndexFile sandboxDir)

addSandboxSource :: Directory -> EitherT MafiaViolation IO ()
addSandboxSource dir = do
  rel <- fromMaybe dir <$> makeRelativeToCurrentDirectory dir
  liftIO (T.hPutStrLn stderr ("Sandbox: Adding " <> rel))
  sandbox_ "add-source" [dir]

removeSandboxSource :: Directory -> EitherT MafiaViolation IO ()
removeSandboxSource dir = do
  rel <- fromMaybe dir <$> makeRelativeToCurrentDirectory dir
  liftIO (T.hPutStrLn stderr ("Sandbox: Removing " <> rel))
  sandbox_ "delete-source" ["-v0", dir]

getSandboxSources :: EitherT MafiaViolation IO (Set Directory)
getSandboxSources = do
  Out sources <- sandbox "list-sources" []

  let dropHeader = drop 3
      dropFooter = reverse . drop 2 . reverse

  return . Set.fromList
         . dropFooter
         . dropHeader
         . T.lines
         $ sources

getSubmoduleSources :: EitherT MafiaViolation IO (Set Directory)
getSubmoduleSources = Set.union <$> getConfiguredSources
                                <*> liftGit getConventionSources

getConfiguredSources :: EitherT MafiaViolation IO (Set Directory)
getConfiguredSources = do
  root <- liftGit getProjectRoot
  name <- firstEitherT MafiaProjectError getProjectName
  cfg  <- readUtf8 (name <> ".submodules")
  return . Set.fromList
         . fmap (root </>)
         . T.lines
         . fromMaybe T.empty
         $ cfg

getConventionSources :: EitherT GitError IO (Set Directory)
getConventionSources = do
  root <- getProjectRoot
  subs <- fmap ((root </>) . subName) <$> getSubmodules
  Set.unions <$> traverse getSourcesFrom subs

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
