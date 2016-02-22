{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Mafia.Cabal.Constraint (
    Constraint(..)
  , renderConstraint
  , parseConstraint
  , constraintArgs

  , packageRefConstraints
  , sourcePackageConstraint
  ) where

import           Data.Text (Text)
import qualified Data.Text as T

import           Mafia.Cabal.Package
import           Mafia.Cabal.Types
import           Mafia.Package
import           Mafia.Process

import           P


data Constraint =
    ConstraintPackage !PackageId
  | ConstraintFlag !PackageName !Flag
    deriving (Eq, Ord, Show)

renderConstraint :: Constraint -> Text
renderConstraint = \case
  ConstraintPackage (PackageId name ver) ->
    unPackageName name <> " == " <> renderVersion ver
  ConstraintFlag name flag ->
    unPackageName name <> " " <> renderFlag flag

parseConstraint :: Text -> Either CabalError Constraint
parseConstraint txt =
  case T.words txt of
    [name, "==", ver] ->
      case parseVersion ver of
        Nothing ->
          Left $ CabalCouldNotParseVersion ver
        Just v ->
          Right . ConstraintPackage $ PackageId (mkPackageName name) v
    [name, flag] ->
      case parseFlag flag of
        Nothing ->
          Left $ CabalCouldNotParseFlag flag
        Just f ->
          Right $ ConstraintFlag (mkPackageName name) f
    _ ->
      Left $ CabalCouldNotParseConstraint txt

constraintArgs :: [Constraint] -> [Argument]
constraintArgs =
  concatMap (\c -> ["--constraint", renderConstraint c])

packageRefConstraints :: PackageRef -> [Constraint]
packageRefConstraints = \case
  PackageRef pid@(PackageId name _) flags _ ->
    [ConstraintPackage pid] <> fmap (ConstraintFlag name) flags

sourcePackageConstraint :: SourcePackage -> Constraint
sourcePackageConstraint =
  ConstraintPackage . spPackageId
