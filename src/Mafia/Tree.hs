{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Mafia.Tree (
    renderTree
  ) where

import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text.Lazy as Lazy

import           Mafia.Cabal

import           P hiding (Last)


data Loc =
    Mid
  | Last

mapLoc :: (Loc -> a -> b) -> [a] -> [b]
mapLoc f = \case
  [] ->
    []
  x : [] ->
    f Last x : []
  x : xs ->
    f Mid x : mapLoc f xs

renderTree' :: Maybe Lazy.Text -> Loc -> Package -> Lazy.Text
renderTree' mindent loc = \case
  Package ref deps _ ->
    let
      sorted =
        Set.toList deps

      branch
        | mindent == Nothing =
          Lazy.empty
        | otherwise =
          case loc of
            Mid  -> " ├─╼ "
            Last -> " └─╼ "

      indent =
        fromMaybe Lazy.empty mindent

      tindent
        | mindent == Nothing =
          Just Lazy.empty
        | otherwise =
          case loc of
            Mid  -> Just (indent <> " │   ")
            Last -> Just (indent <> "     ")
    in
      indent <>
      branch <>
      Lazy.fromStrict (renderPackageRef ref) <>
      Lazy.concat (mapLoc (\l d -> "\n" <> renderTree' tindent l d) sorted)

renderTree :: Set Package -> Lazy.Text
renderTree =
  Lazy.unlines . mapLoc (renderTree' Nothing) . Set.toList
