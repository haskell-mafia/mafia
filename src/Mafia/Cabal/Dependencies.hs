{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module Mafia.Cabal.Dependencies
  ( findDependencies

    -- exported for testing
  , PackagePlan(..)
  , PackageStatus(..)
  , PackageChange(..)
  , parsePackagePlan
  , renderPackagePlan
  ) where

import           Control.Monad.IO.Class (liftIO)

import           Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import qualified Data.Graph as Graph
import qualified Data.List as List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.String (String)
import           Data.Text (Text)
import qualified Data.Text as T

import           Mafia.Cabal.Index
import           Mafia.Cabal.Package
import           Mafia.Cabal.Process (cabalFrom)
import           Mafia.Cabal.Types
import           Mafia.Cabal.Version
import           Mafia.IO
import           Mafia.Package
import           Mafia.Path
import           Mafia.Process

import           P

import           System.IO (IO)
import           System.IO.Temp (withSystemTempDirectory)

import           X.Control.Monad.Trans.Either

------------------------------------------------------------------------

findDependencies :: [SourcePackage] -> EitherT CabalError IO [Package]
findDependencies spkgs = do
  fromInstallPlan spkgs <$> calculateInstallPlan spkgs

fromInstallPlan :: [SourcePackage] -> [PackagePlan] -> [Package]
fromInstallPlan spkgs rdeps =
  let rdMap =
        mapFromList (refId . ppRef) rdeps

      spCombine s r =
        r { ppRef = (ppRef r) { refSrcPkg = Just s } }

      spMap =
        mapFromList spPackageId spkgs

      combinedRevDeps =
        Map.intersectionWith spCombine spMap rdMap `Map.union` rdMap

      (graph, fromVertex0) =
        Graph.graphFromEdges' (fmap toGraphKey (Map.elems combinedRevDeps))

      fromVertex =
        fromGraphKey . fromVertex0

      packageRefs =
        fmap fromVertex (Graph.topSort graph)

      dependencies =
        reifyPackageRefs $
        Map.unionsWith (<>) $
        fmap (\(k,v) -> Map.fromList [(k, [v]), (v, [])]) $
        fmap (bimap fromVertex fromVertex) $
        Graph.edges $
        Graph.transposeG $
        graph

      lookupRef ref =
        fromMaybe (mkPackage ref []) (Map.lookup ref dependencies)

  in fmap lookupRef packageRefs

reifyPackageRefs :: Map PackageRef [PackageRef] -> Map PackageRef Package
reifyPackageRefs refs =
  let pkgs =
        Map.mapWithKey lookup refs
      lookup ref deps =
        mkPackage ref (mapMaybe (\d -> Map.lookup d pkgs) deps)
  in pkgs

mapFromList :: Ord k => (v -> k) -> [v] -> Map k v
mapFromList f xs = Map.fromList (List.zip (fmap f xs) xs)

toGraphKey :: PackagePlan -> (PackagePlan, PackageId, [PackageId])
toGraphKey pp = (pp, refId (ppRef pp), ppDeps pp)

fromGraphKey :: (PackagePlan, PackageId, [PackageId]) -> PackageRef
fromGraphKey (pp, _, _) = ppRef pp

calculateInstallPlan :: [SourcePackage] -> EitherT CabalError IO [PackagePlan]
calculateInstallPlan spkgs = do
  checkCabalVersion

  EitherT . withSystemTempDirectory "mafia-deps-" $ \tmp0 -> runEitherT $ do
    dir <- getCurrentDirectory

    let tmp   = T.pack tmp0
        cabal = cabalFrom dir (Just (tmp </> "sandbox.config"))

    Hush <- cabal "sandbox" ["init", "--sandbox", tmp]

    -- this is a fast 'cabal sandbox add-source'
    createIndexFile (fmap spDirectory spkgs) tmp

    -- make sure we're installing the source package by
    -- pinning its version explicitly
    let constraints =
          concatMap spConstraintArgs spkgs

        installDryRun args =
          cabal "install" $
            [ "--only-dependencies"
            , "--force-reinstalls"
            , "--enable-tests"
            , "--enable-benchmarks"
            , "--enable-profiling"
            , "--reorder-goals"
            , "--max-backjumps=-1"
            , "--dry-run" ] <> constraints <> args

    result <- liftIO . runEitherT $ installDryRun ["-v2"]
    case result of
      Right (OutErr out _) ->
        hoistEither (parseInstallPlan out)
      Left _ -> do
        -- this will fail with the standard cabal dependency error message
        Pass <- installDryRun []
        -- this should never happen
        left CabalInstallIsNotReferentiallyTransparent

spConstraintArgs :: SourcePackage -> [Text]
spConstraintArgs sp =
  let pid  = spPackageId sp
      name = unPackageName (pkgName pid)
      ver  = renderVersion (pkgVersion pid)
  in [ "--constraint", name <> " == " <> ver ]

------------------------------------------------------------------------

data PackageChange =
  PackageChange {
      pcPackageId  :: PackageId
    , pcNewVersion :: Version
    } deriving (Eq, Ord, Show)

data PackageStatus =
    NewPackage
  | NewVersion
  | Reinstall [PackageChange]
    deriving (Eq, Ord, Show)

data PackagePlan =
  PackagePlan {
      ppRef    :: PackageRef
    , ppLatest :: Maybe Version
    , ppDeps   :: [PackageId]
    , ppStatus :: PackageStatus
    } deriving (Eq, Ord, Show)

parseInstallPlan :: Text -> Either CabalError [PackagePlan]
parseInstallPlan =
  first (CabalParseError . T.pack) .
  traverse parsePackagePlan .
  List.drop 1 .
  List.dropWhile (/= "In order, the following would be installed:") .
  T.lines

parsePackagePlan :: Text -> Either String PackagePlan
parsePackagePlan txt =
  let go err = "Invalid package plan: " <> T.unpack txt <> "\nExpected: " <> err
  in first go (A.parseOnly pPackagePlan txt)

renderPackagePlan :: PackagePlan -> Text
renderPackagePlan (PackagePlan (PackageRef pid fs _) latest deps status) =
  mconcat
   [ renderPackageId pid
   , case latest of
       Nothing  -> ""
       Just ver -> " (latest: " <> renderVersion ver <> ")"
   , mconcat $ fmap (\f -> " " <> renderFlag f) fs
   , case deps of
       [] -> ""
       _  -> " (via: " <> T.intercalate " " (fmap renderPackageId deps) <> ")"
   , " " <> renderPackageStatus status
   ]

renderPackageStatus :: PackageStatus -> Text
renderPackageStatus = \case
  NewPackage ->
    "(new package)"
  NewVersion ->
    "(new version)"
  Reinstall cs ->
    "(reinstall) (changes: " <> T.intercalate ", " (fmap renderPackageChange cs) <> ")"

renderPackageChange :: PackageChange -> Text
renderPackageChange = \case
  PackageChange pid ver ->
    renderPackageId pid <> " -> " <> renderVersion ver

pPackagePlan :: Parser PackagePlan
pPackagePlan = do
  pid    <- pPackageId (== ' ')
  latest <- optional pLatest
  flags  <- many pFlag
  deps   <- fromMaybe [] <$> optional pVia
  status <- pNewPackage <|> pNewVersion <|> pReinstall
  ()     <- A.endOfInput
  pure (PackagePlan (PackageRef pid flags Nothing) latest deps status)

pFlag :: Parser Flag
pFlag =
  let flag p = A.char ' ' *> A.char p *> A.takeTill (== ' ')
  in FlagOff <$> flag '-' <|>
     FlagOn  <$> flag '+'

pLatest :: Parser Version
pLatest =
  A.string " (latest: " *> pVersion (== ')') <* A.char ')'

pVia :: Parser [PackageId]
pVia = do
  let pkg = pPackageId (\x -> x == ' ' || x == ')')
  A.string " (via: " *> pkg `A.sepBy1` A.char ' ' <* A.char ')'

pNewPackage :: Parser PackageStatus
pNewPackage =
  A.string " (new package)" *> pure NewPackage

pNewVersion :: Parser PackageStatus
pNewVersion =
  A.string " (new version)" *> pure NewVersion

pReinstall :: Parser PackageStatus
pReinstall = do
  _  <- A.string " (reinstall) (changes: "
  cs <- pPackageChange `A.sepBy1` A.string ", "
  _  <- A.char ')'
  pure (Reinstall cs)

pPackageChange :: Parser PackageChange
pPackageChange =
  let pkg = pPackageId (== ' ')
      arr = A.string " -> "
      ver = pVersion (\x -> x == ',' || x == ')')
  in PackageChange <$> pkg <*> (arr *> ver)


-- TODO would be good if parsePackageId/parseVersion were attoparsec parsers
-- TODO instead of `Text -> Maybe a` so we didn't need these two clunky
-- TODO wrappers below:

pPackageId :: (Char -> Bool) -> Parser PackageId
pPackageId p = do
  txt <- A.takeTill p
  case parsePackageId txt of
    Nothing  -> fail ("not a package-id: " <> T.unpack txt)
    Just pid -> pure pid

pVersion :: (Char -> Bool) -> Parser Version
pVersion p = do
  txt <- A.takeTill p
  case parseVersion txt of
    Nothing  -> fail ("not a version number: " <> T.unpack txt)
    Just ver -> pure ver
