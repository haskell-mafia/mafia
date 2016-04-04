{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Mafia.Cabal.Types where

import           Disorder.Core.Tripping (tripping)

import           Mafia.Cabal.Types

import           P

import           Test.Mafia.Arbitrary ()
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()


prop_roundtrip_Flag =
  tripping renderFlag (parseFlag :: Text -> Maybe Flag)

return []
tests = $quickCheckAll
