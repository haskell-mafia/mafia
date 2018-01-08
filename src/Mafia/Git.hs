{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Mafia.Git
    ( GitError(..)
    , renderGitError
    , getProjectRoot

    , SubmoduleState
    , Submodule(..)
    , subNeedsInit
    , initSubmodules
    , getSubmodules
    ) where

import           Control.Monad.IO.Class (liftIO)

import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Mafia.Path
import           Mafia.Process

import           Mafia.P

import           System.IO (IO, stderr)

import           X.Control.Monad.Trans.Either (EitherT, left)

------------------------------------------------------------------------

data GitError =
    GitProcessError ProcessError
  | GitParseError Text
    deriving (Show)

data SubmoduleState =
    NeedsInit
  | OutOfSync
  | Ready
    deriving (Eq, Ord, Show)

data Submodule =
  Submodule {
      subState :: SubmoduleState
    , subName  :: Directory
    } deriving (Eq, Ord, Show)

subNeedsInit :: Submodule -> Bool
subNeedsInit =
  (== NeedsInit) . subState

subOutOfSync :: Submodule -> Bool
subOutOfSync =
  (== OutOfSync) . subState

renderGitError :: GitError -> Text
renderGitError = \case
  GitProcessError e ->
    renderProcessError e

  GitParseError msg ->
    "Parse failed: " <> msg

------------------------------------------------------------------------

getProjectRoot :: EitherT GitError IO File
getProjectRoot =
  T.strip . unOut <$> call GitProcessError "git" ["rev-parse", "--show-toplevel"]

------------------------------------------------------------------------

-- Init any submodules that we need. Don't worry about explicit submodules
-- file here, we just want to trust git to tell us things that haven't been
-- initialized, we really _don't_ want to run this just because a module is
-- dirty from development changes etc...
initSubmodules :: EitherT GitError IO ()
initSubmodules = do
  root <- getProjectRoot
  nss  <- fmap subName . filter subNeedsInit <$> getSubmodules
  oss  <- fmap subName . filter subOutOfSync <$> getSubmodules

  forM_ nss $ \s -> do
    PassErr <- callFrom GitProcessError root "git" ["submodule", "update", "--init", s]
    return ()

  forM_ oss $ \s ->
    liftIO . T.hPutStrLn stderr $ "Warning: " <> s <> " is out of sync with the git index"

getSubmodules :: EitherT GitError IO [Submodule]
getSubmodules = do
  root    <- getProjectRoot
  Out out <- callFrom GitProcessError root "git" ["submodule"]

  sequence . fmap parseSubmoduleLine . T.lines $ out

parseSubmoduleLine :: Text -> EitherT GitError IO Submodule
parseSubmoduleLine line = Submodule (parseSubmoduleState line) <$> parseSubmoduleName line

parseSubmoduleState :: Text -> SubmoduleState
parseSubmoduleState line
  | "-" `T.isPrefixOf` line = NeedsInit
  | "+" `T.isPrefixOf` line = OutOfSync
  | otherwise               = Ready

parseSubmoduleName :: Text -> EitherT GitError IO Text
parseSubmoduleName line =
  case T.words line of
    (_:x:_) -> pure x
    _       -> left (GitParseError ("failed to read submodule name from: " <> line))
