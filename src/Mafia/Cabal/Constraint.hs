{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Mafia.Cabal.Constraint (
    Constraint(..)
  , Bound(..)
  , renderConstraint
  , parseConstraint
  , constraintArgs

  , packageRefConstraints
  , sourcePackageConstraint
  ) where

import qualified Data.Text as T

import           Mafia.Cabal.Types
import           Mafia.P
import           Mafia.Package
import           Mafia.Process

data Constraint =
    ConstraintPackage !PackageName !Version
  | ConstraintBounded !PackageName !Bound !(Maybe Bound)
  | ConstraintFlag !PackageName !Flag
    deriving (Eq, Ord, Show)

data Bound =
    Exclusive !Version
  | Inclusive !Version
    deriving (Eq, Ord, Show)

renderConstraint :: Constraint -> Text
renderConstraint = \case
  ConstraintPackage name ver ->
    unPackageName name <> " == " <> renderVersion ver
  ConstraintBounded name lower Nothing ->
    unPackageName name <> " " <> renderLowerBound lower
  ConstraintBounded name lower (Just upper) ->
    unPackageName name <> " " <> renderLowerBound lower <> " && " <> renderUpperBound upper
  ConstraintFlag name flag ->
    unPackageName name <> " " <> renderFlag flag

renderLowerBound :: Bound -> Text
renderLowerBound = \case
  Exclusive ver ->
    "> " <> renderVersion ver
  Inclusive ver ->
    ">= " <> renderVersion ver

renderUpperBound :: Bound -> Text
renderUpperBound = \case
  Exclusive ver ->
    "< " <> renderVersion ver
  Inclusive ver ->
    "<= " <> renderVersion ver

parseConstraint :: Text -> Either CabalError Constraint
parseConstraint txt =
  case T.words txt of
    [name, "==", ver] ->
      ConstraintPackage (mkPackageName name)
        <$> parseVersion' ver

    [name, lowerSym, lowerVer] ->
      ConstraintBounded (mkPackageName name)
        <$> parseLowerBound lowerSym lowerVer
        <*> pure Nothing

    [name, lowerSym, lowerVer, "&&", upperSym, upperVer] ->
      ConstraintBounded (mkPackageName name)
        <$> parseLowerBound lowerSym lowerVer
        <*> (Just <$> parseUpperBound upperSym upperVer)

    [name, flag] ->
      case parseFlag flag of
        Nothing ->
          Left $ CabalCouldNotParseFlag flag
        Just f ->
          Right $ ConstraintFlag (mkPackageName name) f

    _ ->
      Left $ CabalCouldNotParseConstraint txt

parseVersion' :: Text -> Either CabalError Version
parseVersion' ver =
  case parseVersion ver of
    Nothing ->
      Left $ CabalCouldNotParseVersion ver
    Just v ->
      Right v

parseLowerBound :: Text -> Text -> Either CabalError Bound
parseLowerBound sym ver =
  case sym of
    ">" ->
      Exclusive <$> parseVersion' ver
    ">=" ->
      Inclusive <$> parseVersion' ver
    _ ->
      Left $ CabalCouldNotParseLowerBound sym ver

parseUpperBound :: Text -> Text -> Either CabalError Bound
parseUpperBound sym ver =
  case sym of
    "<" ->
      Exclusive <$> parseVersion' ver
    "<=" ->
      Inclusive <$> parseVersion' ver
    _ ->
      Left $ CabalCouldNotParseUpperBound sym ver

constraintArgs :: [Constraint] -> [Argument]
constraintArgs =
  concatMap (\c -> ["--constraint", renderConstraint c])

packageRefConstraints :: PackageRef -> [Constraint]
packageRefConstraints = \case
  PackageRef (PackageId name ver) flags _ ->
    [ConstraintPackage name ver] <> fmap (ConstraintFlag name) flags

sourcePackageConstraint :: SourcePackage -> Constraint
sourcePackageConstraint spkg =
  case spPackageId spkg of
    PackageId name ver ->
      ConstraintPackage name ver
