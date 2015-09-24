{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Mafia.Path
  ( -- * Types
    Path
  , File
  , Directory

    -- * Operations
  , (</>)
  , takeExtension
  , dropExtension
  ) where

import           Data.Text (Text)
import qualified Data.Text as T

import           P

import qualified System.FilePath as FilePath

------------------------------------------------------------------------
-- Types

type Path      = Text
type File      = Path
type Directory = Path

------------------------------------------------------------------------
-- Operations

(</>) :: Path -> Path -> Path
(</>) x y = T.pack (T.unpack x FilePath.</> T.unpack y)

takeExtension :: Path -> Text
takeExtension = T.pack . FilePath.takeExtension . T.unpack

dropExtension :: Path -> Path
dropExtension = T.pack . FilePath.dropExtension . T.unpack
