{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Mafia.Cabal.Sandbox
  ( sandbox
  , sandbox_
  , initSandbox

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
import           Mafia.Project

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
initSandbox :: EitherT CabalError IO Directory
initSandbox = do
  name <- firstEitherT CabalProjectError getProjectName

  sandboxBase   <- fromMaybe ".cabal-sandbox" <$> liftIO (readUtf8 (name <> ".sandbox"))
  ghcVer        <- firstEitherT CabalGhcError getGhcVersion
  cfgSandboxDir <- liftIO (runEitherT getSandboxDir)

  let dir = sandboxBase </> ghcVer

  let cfgOk = dropLeft cfgSandboxDir == Just dir
  dirOk <- doesDirectoryExist dir

  unless (cfgOk && dirOk) $
    call_ CabalProcessError "cabal" ["sandbox", "--sandbox", dir, "init"]

  return dir

dropLeft :: Either a b -> Maybe b
dropLeft =
  either (const Nothing) Just

------------------------------------------------------------------------

defaultConfig :: SandboxConfigFile
defaultConfig =
  "cabal.sandbox.config"

getSandboxDir :: EitherT CabalError IO Directory
getSandboxDir = do
  dir <- readSandboxDir defaultConfig
  liftIO (tryMakeRelativeToCurrent dir)

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
