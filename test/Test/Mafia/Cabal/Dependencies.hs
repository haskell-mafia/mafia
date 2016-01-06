{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Mafia.Cabal.Dependencies where

import           Disorder.Core.Tripping (tripping)

import           Mafia.Cabal.Dependencies

import           P

import           Test.Mafia.Arbitrary ()
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()


prop_roundtrip_RevDeps =
  tripping renderRevDeps parseRevDeps

return []
tests = $quickCheckAll
