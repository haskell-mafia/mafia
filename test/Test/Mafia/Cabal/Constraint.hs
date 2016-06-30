{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Mafia.Cabal.Constraint where

import           Disorder.Core.Tripping (tripping)

import           Mafia.Cabal.Constraint

import           P

import           System.IO (IO)

import           Test.Mafia.Arbitrary (EqCabalError(..))
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()


prop_roundtrip_Constraint :: Constraint -> Property
prop_roundtrip_Constraint =
  tripping renderConstraint (first EqCabalError . parseConstraint)

return []
tests :: IO Bool
tests =
  $quickCheckAll
