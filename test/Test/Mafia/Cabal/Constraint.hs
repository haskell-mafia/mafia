{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Mafia.Cabal.Constraint where

import           Disorder.Core.Tripping (tripping)

import           Mafia.Cabal.Constraint

import           P

import           Test.Mafia.Arbitrary (EqCabalError(..))
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()


prop_roundtrip_Constraint =
  tripping renderConstraint (first EqCabalError . parseConstraint)

return []
tests = $quickCheckAll
