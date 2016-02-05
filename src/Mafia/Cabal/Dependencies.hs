{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Mafia.Cabal.Dependencies
  ( filterPackages
  , findDependencies
  , flagArg

    -- exported for testing
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
import           Mafia.Ghc
import           Mafia.IO
import           Mafia.Package
import           Mafia.Path
import           Mafia.Process

import           P

import           System.IO (IO)

import           X.Control.Monad.Trans.Either

------------------------------------------------------------------------

filterPackages :: PackageName -> [Package] -> [Package]
filterPackages name pkgs =
  mapMaybe (filterPackage name) pkgs

filterPackage :: PackageName -> Package -> Maybe Package
filterPackage name = \case
  Package ref deps hash
    | name == pkgName (refId ref) ->
      Just (Package ref [] hash)
    | deps'@(_:_) <- filterPackages name deps ->
      Just (Package ref deps' hash)
    | otherwise ->
      Nothing

------------------------------------------------------------------------

findDependencies :: [Flag] -> [SourcePackage] -> EitherT CabalError IO [Package]
findDependencies flags spkgs = do
  fromInstallPlan spkgs <$> calculateInstallPlan flags spkgs

fromInstallPlan :: [SourcePackage] -> [PackagePlan] -> [Package]
fromInstallPlan spkgs rdeps =
  let rdMap =
        mapFromList (refId . ppRef) rdeps

      topLevels =
        fmap ppRef $ filter (null . ppDeps) rdeps

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

  in
    concatMap pkgDeps .
    fmap lookupRef $
    List.intersectBy ((==) `on` refId) packageRefs topLevels

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

calculateInstallPlan :: [Flag] -> [SourcePackage] -> EitherT CabalError IO [PackagePlan]
calculateInstallPlan flags spkgs = do
  (_ :: GhcVersion) <- firstT CabalGhcError getGhcVersion -- check ghc is on the path
  checkCabalVersion

  withSystemTempDirectory "mafia-deps-" $ \tmp -> do
    dir <- getCurrentDirectory

    let cabal = cabalFrom dir (Just (tmp </> "sandbox.config"))

    Hush <- cabal "sandbox" ["init", "--sandbox", tmp]

    -- this is a fast 'cabal sandbox add-source'
    createIndexFile (fmap spDirectory spkgs) tmp

    -- make sure we're installing the source package by
    -- pinning its version explicitly
    let constraints =
          concatMap spConstraintArgs spkgs

        flagArgs =
          fmap flagArg flags

        installDryRun args =
          cabal "install" $
            [ "--enable-tests"
            , "--enable-benchmarks"
            , "--enable-profiling"
            , "--reorder-goals"
            , "--max-backjumps=-1"
            , "--avoid-reinstalls"
            , "--dry-run" ] <> flagArgs <> constraints <> args

    result <- liftIO . runEitherT $ installDryRun ["-v2"]
    case result of
      Right (OutErr out _) -> do
        plan <- hoistEither (parseInstallPlan out)
        case mapMaybe takeReinstall plan of
          [] -> return plan
          xs -> left (CabalReinstallsDetected xs)
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

flagArg :: Flag -> Argument
flagArg = \case
  FlagOff f ->
    "--flags=-" <> f
  FlagOn f ->
    "--flags=" <> f

takeReinstall :: PackagePlan -> Maybe PackagePlan
takeReinstall p =
  case p of
    PackagePlan _ _ _ (Reinstall _) -> Just p
    PackagePlan _ _ _ _             -> Nothing

------------------------------------------------------------------------

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

pPackagePlan :: Parser PackagePlan
pPackagePlan = do
  pid    <- pPackageId (== ' ')
  latest <- optional pLatest
  flags  <- many pFlag
  _      <- many pStanza
  deps   <- fromMaybe [] <$> optional pVia
  status <- pNewPackage <|> pNewVersion <|> pReinstall
  ()     <- A.endOfInput
  pure (PackagePlan (PackageRef pid flags Nothing) latest deps status)

pFlag :: Parser Flag
pFlag =
  let flag p = A.char ' ' *> A.char p *> A.takeTill (== ' ')
  in FlagOff <$> flag '-' <|>
     FlagOn  <$> flag '+'

pStanza :: Parser ()
pStanza =
  A.string " *test" *> pure () <|>
  A.string " *bench" *> pure ()

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
