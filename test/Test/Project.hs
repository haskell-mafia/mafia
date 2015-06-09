{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Project where

import           P
import           Test.QuickCheck

prop_function1 (n :: Int) =
  n === n

return []
tests :: IO Bool
tests = $quickCheckAll
