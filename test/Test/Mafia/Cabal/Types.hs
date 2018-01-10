{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Mafia.Cabal.Types where

import           Disorder.Core.Tripping (tripping)

import           Mafia.Cabal.Types

import           Mafia.P

import           System.IO (IO)

import           Test.Mafia.Arbitrary ()
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()


prop_roundtrip_Flag :: Flag -> Property
prop_roundtrip_Flag =
  tripping renderFlag (parseFlag :: Text -> Maybe Flag)

return []
tests :: IO Bool
tests =
  $quickCheckAll
