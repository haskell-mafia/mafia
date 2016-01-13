{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Mafia.Cabal.Sandbox
  ( sandbox
  , sandbox_
  , initSandbox
  , removeSandbox

  , getSandboxDir
  , getPackageDB
  , readSandboxDir
  , readPackageDB
  ) where

import           Control.Monad.IO.Class (MonadIO(..))

import qualified Data.List as List
import           Data.Text (Text)
import qualified Data.Text as T

import           Mafia.Cabal.Process
import           Mafia.Cabal.Types
import           Mafia.Ghc
import           Mafia.IO
import           Mafia.Path
import           Mafia.Process

import           P

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT, runEitherT, firstEitherT, left)


sandbox :: ProcessResult a => Argument -> [Argument] -> EitherT CabalError IO a
sandbox cmd args = do
  _ <- initSandbox
  cabal "sandbox" (cmd:args)

sandbox_ :: Argument -> [Argument] -> EitherT CabalError IO ()
sandbox_ cmd args = do
  Pass <- sandbox cmd args
  return ()

-- Sandbox initialized if required, this should support sandboxes in parent
-- directories.
initSandbox :: EitherT CabalError IO SandboxDir
initSandbox = do
  sandboxDir <- getInferredSandboxDir
  cfgSandboxDir <- liftIO (runEitherT getSandboxDir)
  let cfgOk = dropLeft cfgSandboxDir == Just sandboxDir

  dirOk <- doesDirectoryExist sandboxDir

  unless (cfgOk && dirOk) $
    call_ CabalProcessError "cabal" ["sandbox", "--sandbox", sandboxDir, "init"]

  return sandboxDir

dropLeft :: Either a b -> Maybe b
dropLeft =
  either (const Nothing) Just

removeSandbox :: EitherT CabalError IO ()
removeSandbox = do
  dir <- getInferredSandboxDir

  -- remove sandbox directory
  whenM (doesDirectoryExist dir) $
    removeDirectoryRecursive dir

  -- remvoe sandbox config file
  whenM (doesFileExist dir) $
    removeFile defaultConfig

  -- remove root sandbox directory if it is now empty
  whenM (doesDirectoryExist sandboxRoot) $ do
    entries <- getDirectoryContents sandboxRoot
    when (null entries) $
      removeDirectoryRecursive sandboxRoot

------------------------------------------------------------------------

defaultConfig :: SandboxConfigFile
defaultConfig =
  "cabal.sandbox.config"

sandboxRoot :: Directory
sandboxRoot =
  ".cabal-sandbox"

-- | The location where the sandbox is inferred to be (based on the GHC version)
getInferredSandboxDir :: EitherT CabalError IO SandboxDir
getInferredSandboxDir = do
  ghcVer <- firstEitherT CabalGhcError getGhcVersion
  return $ sandboxRoot </> ghcVer

-- | The location where the sandbox is configured to be (based on the cabal.sandbox.config)
getSandboxDir :: EitherT CabalError IO SandboxDir
getSandboxDir = do
  dir <- readSandboxDir defaultConfig
  liftIO (tryMakeRelativeToCurrent dir)

-- | The location where the package database is configured to be (based on the cabal.sandbox.config)
getPackageDB :: EitherT CabalError IO Directory
getPackageDB = do
  dir <- readPackageDB defaultConfig
  liftIO (tryMakeRelativeToCurrent dir)

------------------------------------------------------------------------

readSandboxDir :: SandboxConfigFile -> EitherT CabalError IO Directory
readSandboxDir cfg =
  readConfigField cfg "prefix"

readPackageDB :: SandboxConfigFile -> EitherT CabalError IO Directory
readPackageDB cfg =
  readConfigField cfg "package-db"

readConfigField :: SandboxConfigFile -> Text -> EitherT CabalError IO Text
readConfigField file field = do
  mcfg <- readUtf8 file
  case mcfg of
    Nothing  -> left (CabalSandboxConfigFileNotFound file)
    Just cfg -> do
      let lines  = fmap T.strip (T.lines cfg)
          prefix = field <> ":"
      case List.dropWhile (not . T.isPrefixOf prefix) lines of
        []       -> left (CabalSandboxConfigFieldNotFound file field)
        (line:_) -> do
          let value = T.strip (T.drop (T.length prefix) line)
          return value
