{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Project.IO where

import           System.IO
import           P
import           Disorder.Core.IO

import           Test.Project.Arbitrary ()

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

prop_function1 (name :: Int) = testIO $
  pure (name === name)

return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs { maxSuccess = 5 })
