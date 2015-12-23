{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Mafia.Hoogle where

import           Mafia.Hoogle
import           Mafia.Package

import           P

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()


prop_joinHooglePackages =
  once $ joinHooglePackages
    (HooglePackagesCached [packageId "a" [5, 6], packageId "b" [3, 1], packageId "b" [3, 4]])
    (HooglePackagesSandbox [packageId "a" [1, 2], packageId "c" [3, 4]])
      === [packageId "a" [1, 2], packageId "c" [3, 4], packageId "b" [3, 4]]


return []
tests = $quickCheckAll
