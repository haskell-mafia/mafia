{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Mafia.Cabal.Dependencies where

import           Test.Mafia.Tripping (tripping)

import           Mafia.Cabal.Dependencies
import           Mafia.Cabal.Types

import           Mafia.P

import           System.IO (IO)

import           Test.Mafia.Arbitrary ()
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()


prop_roundtrip_PackagePlan :: PackagePlan -> Property
prop_roundtrip_PackagePlan =
  tripping renderPackagePlan parsePackagePlan

return []
tests :: IO Bool
tests =
  $quickCheckAll
