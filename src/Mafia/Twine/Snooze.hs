{-# LANGUAGE NoImplicitPrelude #-}
module Mafia.Twine.Snooze (
    snooze
  , Duration (..)
  , toMicroseconds
  , toMilliseconds
  , toSeconds
  ) where

import           Control.Concurrent

import           P

import           System.IO

-- |
-- A Duration is an abstract type, representing a short delay (in the
-- region of micro-seconds to a few minutes).
--
-- This useful for implementing concurrency and process primitives
-- that need to wait for short periods to ensure fairness (for example).
--
newtype Duration =
  Duration {
      duration :: Int
    } deriving (Eq, Show)

toMicroseconds :: Duration -> Int
toMicroseconds =
  duration

toMilliseconds :: Duration -> Int
toMilliseconds =
  flip div 1000 . toMicroseconds

toSeconds :: Duration -> Int
toSeconds =
  flip div 1000 . toMilliseconds

snooze :: Duration -> IO ()
snooze =
  threadDelay . toMicroseconds
