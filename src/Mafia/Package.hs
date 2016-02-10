{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Mafia.Package
  ( PackageName
  , mkPackageName
  , unPackageName

  , PackageId(..)
  , Version(..)
  , packageId
  , renderPackageId
  , renderVersion
  , packageIdTuple
  , parsePackageId
  , parseVersion
  ) where

import           Data.Aeson (Value(..), ToJSON(..), FromJSON(..))
import           Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import qualified Data.Char as Char
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Version (Version (..))
import qualified Data.Version as Version

import           P

import qualified Text.ParserCombinators.ReadP as Parse


-- Similar to Cabal's Distribution.Package

newtype PackageName =
  PackageName {
      ciPackageName :: CI Text
    } deriving (Eq, Ord, Show)

mkPackageName :: Text -> PackageName
mkPackageName =
  PackageName . CI.mk

unPackageName :: PackageName -> Text
unPackageName =
  CI.original . ciPackageName

data PackageId =
  PackageId {
      pkgName :: !PackageName
    , pkgVersion :: !Version
    } deriving (Eq, Show)

instance Ord PackageId where
  compare (PackageId xn xv) (PackageId yn yv) =
    compare
      (ciPackageName xn, xv)
      (ciPackageName yn, yv)

packageId :: Text -> [Int] -> PackageId
packageId n v =
  PackageId (mkPackageName n) (Version v [])

renderPackageId :: PackageId -> Text
renderPackageId (PackageId name version) =
  unPackageName name <> "-" <> renderVersion version

renderVersion :: Version -> Text
renderVersion =
  T.pack . Version.showVersion

packageIdTuple :: PackageId -> (PackageName, Version)
packageIdTuple (PackageId n v) =
  (n, v)

-- Extract name from `$name-$version`, but consider `unordered-containers-1.2.3`
parsePackageId :: Text -> Maybe PackageId
parsePackageId =
  let parser =
        PackageId
          <$> (PackageName . CI.mk . T.intercalate "-" . fmap T.pack <$> Parse.sepBy1 component (Parse.char '-'))
          <* Parse.char '-'
          <*> Version.parseVersion
          <* Parse.eof
  in parseLongestMatch parser
  where
    component = do
      cs <- Parse.munch1 Char.isAlphaNum
      if all Char.isDigit cs then Parse.pfail else return cs

parseVersion :: Text -> Maybe Version
parseVersion =
  parseLongestMatch Version.parseVersion

parseLongestMatch :: Parse.ReadP a -> Text -> Maybe a
parseLongestMatch p =
  fmap fst . listToMaybe . reverse . Parse.readP_to_S p . T.unpack

------------------------------------------------------------------------

instance ToJSON PackageId where
  toJSON =
    String . renderPackageId

instance FromJSON PackageId where
  parseJSON = \case
    String txt ->
      case parsePackageId txt of
        Nothing  -> fail ("invalid package-id: " <> T.unpack txt)
        Just pid -> pure pid
    _ ->
      mzero
