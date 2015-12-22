{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Mafia.Package
  ( module X
  , PackageName (..)
  , PackageId (..)
  , packageId
  , renderPackageId
  , packageIdTuple
  , parsePackageId
  ) where

import qualified Data.Char as Char
import           Data.Version as X (Version (..), parseVersion, showVersion)
import           Data.Text (Text)
import qualified Data.Text as T

import           P

import qualified Text.ParserCombinators.ReadP  as Parse


-- Similar to Cabal's Distribution.Package

newtype PackageName =
  PackageName {
      unPackageName :: Text
    } deriving (Eq, Ord, Show)

data PackageId =
  PackageId {
      pkgName :: PackageName
    , pkgVersion :: Version
    } deriving (Eq, Show)


packageId :: Text -> [Int] -> PackageId
packageId n v =
  PackageId (PackageName n) (Version v [])

renderPackageId :: PackageId -> Text
renderPackageId (PackageId name version) =
  unPackageName name <> "-" <> (T.pack . showVersion) version

packageIdTuple :: PackageId -> (PackageName, Version)
packageIdTuple (PackageId n v) =
  (n, v)

-- Extract name from `$name-$version`, but consider `unordered-containers-1.2.3`
parsePackageId :: Text -> Maybe PackageId
parsePackageId  =
  let parser =
        PackageId
          <$> (PackageName . T.intercalate "-" . fmap T.pack <$> Parse.sepBy1 component (Parse.char '-'))
          <* Parse.char '-'
          <*> parseVersion
          <* Parse.eof
  in fmap fst . listToMaybe . reverse . Parse.readP_to_S parser . T.unpack
  where
    component = do
      cs <- Parse.munch1 Char.isAlphaNum
      if all Char.isDigit cs then Parse.pfail else return cs
