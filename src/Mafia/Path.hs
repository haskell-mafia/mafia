{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Mafia.Path
  ( -- * Types
    Path
  , File
  , Directory

    -- * Filename/directory functions
  , (</>)
  , takeDirectory

    -- * Extension functions
  , takeExtension
  , dropExtension
  , extension
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
-- Filename/directory functions

(</>) :: Path -> Path -> Path
(</>) x y = T.pack (T.unpack x FilePath.</> T.unpack y)

takeDirectory :: Path -> Directory
takeDirectory = T.pack . FilePath.takeDirectory . T.unpack

------------------------------------------------------------------------
-- Extension functions

takeExtension :: Path -> Text
takeExtension = T.pack . FilePath.takeExtension . T.unpack

dropExtension :: Path -> Path
dropExtension = T.pack . FilePath.dropExtension . T.unpack

extension :: Text -> Path -> Bool
extension ext = (== ext) . takeExtension
