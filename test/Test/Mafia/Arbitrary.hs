{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Mafia.Arbitrary where

import qualified Data.Text as T

import           Disorder.Corpus (muppets, cooking)

import           Mafia.Cabal.Types
import           Mafia.Package

import           P

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()


instance Arbitrary PackageName where
  arbitrary = do
    name <- T.intercalate "-" <$> listOf1 (elements muppets)
    pure (mkPackageName name)

instance Arbitrary Version where
  arbitrary =
    Version <$>
      listOf1 (choose (0, 100)) <*>
      pure []

instance Arbitrary PackageId where
  arbitrary =
    PackageId <$>
      arbitrary <*>
      arbitrary

instance Arbitrary Flag where
  arbitrary =
    oneof
      [ FlagOn  <$> elements cooking
      , FlagOff <$> elements cooking ]

instance Arbitrary PackageRef where
  arbitrary =
    PackageRef <$>
      arbitrary <*>
      arbitrary <*>
      pure Nothing

instance Arbitrary PackageChange where
  arbitrary =
    PackageChange <$>
      arbitrary <*>
      arbitrary

instance Arbitrary PackageStatus where
  arbitrary =
    oneof
      [ pure NewPackage
      , pure NewVersion
      , Reinstall <$> listOf1 arbitrary ]

instance Arbitrary PackagePlan where
  arbitrary =
    PackagePlan <$>
      arbitrary <*>
      arbitrary <*>
      arbitrary <*>
      arbitrary
