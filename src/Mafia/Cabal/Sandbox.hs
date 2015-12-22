{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Mafia.Cabal.Sandbox
  ( sandbox
  , sandbox_
  , initSandbox
  ) where

import           Control.Monad.IO.Class (MonadIO(..))

import qualified Data.List as List
import           Data.Text (Text)
import qualified Data.Text as T

import           Mafia.Cabal.Process
import           Mafia.Cabal.Types
import           Mafia.IO
import           Mafia.Path
import           Mafia.Process
import           Mafia.Project

import           P

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT, firstEitherT, left, runEitherT)


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
  ghcVer        <- getGhcVersion
  cfgSandboxDir <- getConfiguredSandboxDir

  let sandboxDir = sandboxBase </> ghcVer

  let cfgOk = cfgSandboxDir == Just sandboxDir
  dirOk <- doesDirectoryExist sandboxDir

  unless (cfgOk && dirOk) $
    call_ CabalProcessError "cabal" ["sandbox", "--sandbox", sandboxDir, "init"]

  return sandboxDir

getConfiguredSandboxDir :: (Functor m, MonadIO m) => m (Maybe Directory)
getConfiguredSandboxDir = do
  mcfg <- readUtf8 "cabal.sandbox.config"
  case mcfg of
    Nothing  -> return Nothing
    Just cfg -> do
      let lines  = fmap T.strip (T.lines cfg)
          prefix = "prefix:"
      case List.dropWhile (not . T.isPrefixOf prefix) lines of
        []       -> return Nothing
        (line:_) -> do
          let dir = T.strip (T.drop (T.length prefix) line)
          Just <$> tryMakeRelativeToCurrent dir

getGhcVersion :: EitherT CabalError IO Text
getGhcVersion = do
  result <- runEitherT (call CabalProcessError "ghc" ["--version"])
  case result of
    Left  _         -> left CabalGhcNotInstalled
    Right (Out out) ->
      case reverse (T.words out) of
        []      -> left CabalGhcNotInstalled
        (ver:_) -> return ver
