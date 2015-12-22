{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Mafia.Package where

import qualified Data.Text as T

import           Mafia.Package

import           P

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()


prop_parsePackageId =
  forAll genPackageId $ \pkg ->
    parsePackageId (renderPackageId pkg) === Just pkg


genPackageId :: Gen PackageId
genPackageId =
  PackageId
    <$> genPackageName
    <*> genVersion

genPackageName :: Gen PackageName
genPackageName =
  PackageName
    <$> (fmap (T.intercalate "-") . listOf1 . fmap T.pack . listOf1 . oneof) [choose ('a', 'z'), choose ('A', 'Z')]

genVersion :: Gen Version
genVersion =
  Version
    <$> listOf1 (choose (0, 100))
    <*> pure []

return []
tests = $quickCheckAll
