{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Mafia.Tripping where

import           Control.Applicative
import           Data.Monoid
import           Data.Function
import           Test.QuickCheck

import           Prelude

-- | Generalized round-trip property function
tripping :: (Applicative f, Show (f a), Eq (f a)) => (a -> b) -> (b -> f a) -> a -> Property
tripping = trippingOn id

trippingOn :: (Applicative f, Show (f a), Show (f c), Eq (f c)) => (a -> c) -> (a -> b) -> (b -> f a) -> a -> Property
trippingOn f = trippingWith ((===) `on` fmap f)

trippingWith :: (Applicative f, Show (f a)) => (f a -> f a -> Property) -> (a -> b) -> (b -> f a) -> a -> Property
trippingWith prop to fro a =
  let tripped = (fro . to) a
      purea   = pure a
  in counterexample (show tripped <> " /= " <> show purea)
     (prop tripped purea)
