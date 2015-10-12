{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Mafia.Process where

import           Mafia.Process

import           P

import           Test.QuickCheck


prop_clean xs =
  counterexample "contained \\b or \\r" $
  all (`notElem` dirtyChars) (cleanLines [] xs)


dirtyChars :: [Char]
dirtyChars = "\b\r"


return []
tests = $quickCheckAll
