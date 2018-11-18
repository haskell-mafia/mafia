{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Mafia.Package where

import           Test.Mafia.Tripping (tripping)

import           Mafia.Package

import           Mafia.P

import           System.IO (IO)

import           Test.Mafia.Arbitrary ()
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()


prop_roundtrip_PackageId :: PackageId -> Property
prop_roundtrip_PackageId =
  tripping renderPackageId parsePackageId

return []
tests :: IO Bool
tests =
  $quickCheckAll
