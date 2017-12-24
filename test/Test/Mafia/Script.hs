{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Mafia.Script where

import qualified Data.Attoparsec.Text as Atto
import qualified Data.Text as T

import           Mafia.Script

import           P

import           System.IO (IO)

import           Test.QuickCheck
import           Test.QuickCheck.Instances ()


prop_round_trip_submodule_pragma :: Property
prop_round_trip_submodule_pragma =
  forAll genSubmodule $ \ submod ->
    let
      parsed =
        Atto.parseOnly (pSubmodule <* Atto.endOfInput) (renderPragmaSubmodule submod)
    in
      parsed === Right (PragmaSubmodule submod)

genSubmodule :: Gen Submodule
genSubmodule =
  let
    genMaybeLocation =
      frequency
        [ (1, pure Nothing)
        , (4, Just <$> genSubmoduleLocation)
        ]

    genSubmoduleLocation =
      SubmoduleLocation
        <$> elements [Git, Https]
        <*> elements ["github.com", "gitlab.com", "bitbucket.org"]

    genUser =
      elements
        ["tom", "dick", "jean-marc"]

    genRepo =
      elements
        ["a", "xyz", "this-n-that"]

    genCommit =
      frequency
        [ (1, pure Nothing)
        , (8, Just <$> genCommitHash)
        ]

    genCommitHash =
      T.pack <$> vectorOf 16 (elements "0123456789abcdef")

  in
    Submodule <$> genMaybeLocation <*> genUser <*> genRepo <*> genCommit

return []

tests :: IO Bool
tests = $quickCheckAll
