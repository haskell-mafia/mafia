{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Mafia.Package
  ( PackageName
  , mkPackageName
  , unPackageName

  , PackageId(..)
  , Version
  , packageId
  , renderPackageId
  , renderVersion
  , packageIdTuple
  , parsePackageId
  , parseVersion
  , makeVersion
  , versionNumbers
  ) where

import           Data.Aeson (Value(..), ToJSON(..), FromJSON(..))
import           Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import qualified Data.Char as Char
import qualified Data.Text as T

#if MIN_VERSION_Cabal(2,0,0)
import           Distribution.Version (Version, versionNumbers)
import qualified Distribution.Version as DistVersion
#else
import           Data.Version (Version (..))
import qualified Data.Version as DistVersion
#endif

import           P

import qualified Data.Attoparsec.Text as Parse


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
  PackageId (mkPackageName n) (makeVersion v)

renderPackageId :: PackageId -> Text
renderPackageId (PackageId name version) =
  unPackageName name <> "-" <> renderVersion version

renderVersion :: Version -> Text
renderVersion =
  T.pack . DistVersion.showVersion

packageIdTuple :: PackageId -> (PackageName, Version)
packageIdTuple (PackageId n v) =
  (n, v)

-- Extract name from `$name-$version`, but consider `unordered-containers-1.2.3`
parsePackageId :: Text -> Maybe PackageId
parsePackageId =
  let parser =
        PackageId
          <$> (PackageName . CI.mk . T.intercalate "-" <$> Parse.sepBy1 component (Parse.char '-'))
          <* Parse.char '-'
          <*> pVersion
          <* Parse.endOfInput
  in rightToMaybe . Parse.parseOnly parser
  where
    component = do
      cs <- Parse.takeWhile1 Char.isAlphaNum
      if T.all Char.isDigit cs then fail "" else return cs

parseVersion :: Text -> Maybe Version
parseVersion =
  rightToMaybe . Parse.parseOnly (pVersion <* Parse.endOfInput)

pVersion :: Parse.Parser Version
pVersion = makeVersion <$> Parse.sepBy Parse.decimal (Parse.char '.')

makeVersion :: [Int] -> Version
makeVersion xs =
#if MIN_VERSION_Cabal(2,0,0)
  DistVersion.mkVersion xs
#else
  Version xs []
#endif

#if MIN_VERSION_Cabal(2,0,0)
#else
versionNumbers :: Version -> [Int]
versionNumbers (Version xs _) = xs
#endif

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
