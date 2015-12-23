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

import           Data.Text (Text)
import qualified Data.Text as T

import           Mafia.Path
import           Mafia.Process

import           P

import           System.IO (IO)

import           X.Control.Monad.Trans.Either (EitherT, left)

------------------------------------------------------------------------

data GitError =
    GitProcessError ProcessError
  | GitParseError Text
  deriving (Show)

data SubmoduleState = NeedsInit | Ready
  deriving (Eq, Ord, Show)

data Submodule = Submodule
  { subState :: SubmoduleState
  , subName  :: Directory
  } deriving (Eq, Ord, Show)

subNeedsInit :: Submodule -> Bool
subNeedsInit = (== NeedsInit) . subState

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
  ss   <- fmap subName . filter subNeedsInit <$> getSubmodules

  forM_ ss $ \s ->
    callFrom_ GitProcessError root "git" ["submodule", "update", "--init", s]

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
  | otherwise               = Ready

parseSubmoduleName :: Text -> EitherT GitError IO Text
parseSubmoduleName line =
  case T.words line of
    (_:x:_) -> pure x
    _       -> left (GitParseError ("failed to read submodule name from: " <> line))
