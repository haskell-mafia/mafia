{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Mafia.Cabal.Dependencies
  ( findDependenciesForCurrentDirectory
  , findDependenciesForPackage

  , filterPackages
  , flagArg

    -- exported for testing
  , parsePackagePlan
  , renderPackagePlan
  ) where

import           Control.Monad.Trans.Bifunctor (firstT)
import           Control.Monad.Trans.Either (EitherT, hoistEither, runEitherT, left)

import           Data.Attoparsec.Text (Parser)
import qualified Data.Attoparsec.Text as A
import qualified Data.Graph as Graph
import qualified Data.List as List
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.String (String)
import qualified Data.Text as T

import           Mafia.Cabal.Constraint
import           Mafia.Cabal.Index
import           Mafia.Cabal.Package
import           Mafia.Cabal.Process (cabalFrom)
import           Mafia.Cabal.Types
import           Mafia.Cabal.Version
import           Mafia.Ghc
import           Mafia.IO
import           Mafia.P
import           Mafia.Package
import           Mafia.Path
import           Mafia.Process

import           System.IO (IO)

------------------------------------------------------------------------

filterPackages :: PackageName -> Set Package -> Set Package
filterPackages name pkgs =
  Set.fromDistinctAscList . mapMaybe (filterPackage name) $ Set.toAscList pkgs

filterPackage :: PackageName -> Package -> Maybe Package
filterPackage name = \case
  Package ref deps hash
    | name == pkgName (refId ref) ->
      Just (Package ref Set.empty hash)
    | deps' <- filterPackages name deps
    , not (Set.null deps') ->
      Just (Package ref deps' hash)
    | otherwise ->
      Nothing

------------------------------------------------------------------------

findDependenciesForCurrentDirectory :: [Flag] -> [SourcePackage] -> [Constraint] -> EitherT CabalError IO Package
findDependenciesForCurrentDirectory flags spkgs constraints = do
  hoistEither . fromInstallPlan spkgs =<< installPlanForCurrentDirectory flags spkgs constraints

findDependenciesForPackage :: PackageName -> [Constraint] -> EitherT CabalError IO Package
findDependenciesForPackage name constraints = do
  hoistEither . fromInstallPlan [] =<< installPlanForPackage name constraints

fromInstallPlan :: [SourcePackage] -> [PackagePlan] -> Either CabalError Package
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
        Map.fromList .
        fmap (\ref -> (refId ref, ref)) .
        fmap fromVertex $
        Graph.topSort graph

      dependencies =
        reifyPackageRefs $
        Map.unionsWith Set.union $
        fmap (\(k,v) -> Map.fromList [(k, Set.singleton v), (v, Set.empty)]) $
        fmap (bimap fromVertex fromVertex) $
        Graph.edges $
        Graph.transposeG $
        graph

      lookupRef ref =
        fromMaybe (mkPackage ref Set.empty) (Map.lookup ref dependencies)

      topLevels =
        fmap (refId . ppRef) $
        filter (null . ppDeps) rdeps

  in
    case topLevels of
      [] ->
        Left CabalNoTopLevelPackage
      [topLevel] ->
        case fmap lookupRef (Map.lookup topLevel packageRefs) of
          Nothing ->
            Left (CabalTopLevelPackageNotFoundInPlan topLevel)
          Just pkg ->
            Right pkg
      xs ->
        Left (CabalMultipleTopLevelPackages xs)

reifyPackageRefs :: Map PackageRef (Set PackageRef) -> Map PackageRef Package
reifyPackageRefs refs =
  let pkgs =
        Map.mapWithKey lookup refs
      lookup ref deps =
        mkPackage ref (Set.fromList . mapMaybe (\d -> Map.lookup d pkgs) $ Set.toList deps)
  in pkgs

mapFromList :: Ord k => (v -> k) -> [v] -> Map k v
mapFromList f xs = Map.fromList (List.zip (fmap f xs) xs)

toGraphKey :: PackagePlan -> (PackagePlan, PackageId, [PackageId])
toGraphKey pp = (pp, refId (ppRef pp), ppDeps pp)

fromGraphKey :: (PackagePlan, PackageId, [PackageId]) -> PackageRef
fromGraphKey (pp, _, _) = ppRef pp

installPlanForCurrentDirectory :: [Flag] -> [SourcePackage] -> [Constraint] -> EitherT CabalError IO [PackagePlan]
installPlanForCurrentDirectory flags spkgs constraints0 = do
  let
    -- Make sure we can only install the source package by pinning its version
    -- explicitly. This makes cabal fail if the .cabal file would have caused
    -- the hackage version to be installed instead.
    constraints =
      constraintArgs $ constraints0 <> fmap sourcePackageConstraint spkgs

    flagArgs =
      fmap flagArg flags

    args =
      [ "--enable-tests"
      , "--enable-benchmarks"
      , "--enable-profiling" ]

  dir <- getCurrentDirectory
  makeInstallPlan (Just dir) (fmap spDirectory spkgs) (args <> constraints <> flagArgs)

installPlanForPackage :: PackageName -> [Constraint] -> EitherT CabalError IO [PackagePlan]
installPlanForPackage name constraints =
  makeInstallPlan Nothing [] $ [unPackageName name] <> constraintArgs constraints

makeInstallPlan :: Maybe Directory -> [Directory] -> [Argument] -> EitherT CabalError IO [PackagePlan]
makeInstallPlan mdir sourcePkgs installArgs = do
  (_ :: GhcVersion) <- firstT CabalGhcError getGhcVersion -- check ghc is on the path
  checkCabalVersion

  withSystemTempDirectory "mafia-deps-" $ \tmp -> do
    let
      dir = fromMaybe tmp mdir
      cabal = cabalFrom dir (tmp </> "sandbox.config") []

    Hush <- cabal "v1-sandbox" ["init", "--sandbox", tmp]

    -- this is a fast 'cabal sandbox add-source'
    createIndexFile sourcePkgs tmp

    let
      installDryRun args =
        cabal "v1-install" $
          [ "--reorder-goals"
          , "--max-backjumps=-1"
          , "--avoid-reinstalls"
          , "--dry-run" ] <> installArgs <> args

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
  first (CabalInstallPlanParseError . T.pack) .
  A.parseOnly pPackagePlans

parsePackagePlan :: Text -> Either String PackagePlan
parsePackagePlan txt =
  let go err = "Invalid package plan: '" <> T.unpack txt <> "'\nExpected: " <> err
  in first go (A.parseOnly pPackagePlan txt)

pPackagePlans :: Parser [PackagePlan]
pPackagePlans = do
  pDropLines "In order, the following would be installed:"
  A.manyTill (pPackagePlan <* A.char '\n') (A.takeWhile (== '\n') *> A.endOfInput)

-- Drop all lines up to and including the provided target line.
pDropLines :: Text -> Parser ()
pDropLines target =
  let go = do
          l <- A.takeWhile (/= '\n') <* A.char '\n'
          unless (l == target) go
  in go

pPackagePlan :: Parser PackagePlan
pPackagePlan = do
  pid    <- pPackageId
  latest <- optional pLatest
  flags  <- many pFlag
  _      <- many pStanza
  deps   <- fromMaybe [] <$> optional pVia
  status <- pSpaceSep *> (pNewPackage <|> pNewVersion <|> pReinstall)
  pure (PackagePlan (PackageRef pid flags Nothing) latest deps status)

pSpaceSep :: Parser Char
pSpaceSep = A.char '\n' <|> A.char ' '

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
  A.string " (latest:" *> pSpaceSep *> pVersion (== ')') <* A.char ')'

pVia :: Parser [PackageId]
pVia =
  pSpaceSep *> A.string "(via:" *> pSpaceSep *> pPackageId `A.sepBy1` pSpaceSep <* A.char ')'

pNewPackage :: Parser PackageStatus
pNewPackage =
  A.string "(new" *> pSpaceSep *> A.string "package)" *> pure NewPackage

pNewVersion :: Parser PackageStatus
pNewVersion =
  A.string "(new" *> pSpaceSep *> A.string "version)" *> pure NewVersion

pReinstall :: Parser PackageStatus
pReinstall = do
  _  <- A.string "(reinstall)" *> pSpaceSep *> A.string "(changes:" *> pSpaceSep
  cs <- pPackageChange `A.sepBy1` (A.string "," *> pSpaceSep)
  _  <- A.char ')'
  pure (Reinstall cs)

pPackageChange :: Parser PackageChange
pPackageChange =
  let pkg = pPackageId
      arr = A.string " -> "
      ver = pVersion (\x -> x == ',' || x == ')')
  in PackageChange <$> pkg <*> (arr *> ver)


-- TODO would be good if parsePackageId/parseVersion were attoparsec parsers
-- TODO instead of `Text -> Maybe a` so we didn't need these two clunky
-- TODO wrappers below:

pPackageId :: Parser PackageId
pPackageId = do
  let
    isPkgIdChar c
        | c >= 'a' && c <= 'z' = True
        | c >= 'A' && c <= 'Z' = True
        | c >= '0' && c <= '9' = True
        | c == '-' = True
        | c == '.' = True
        | otherwise = False

  txt <- A.takeWhile1 isPkgIdChar
  case parsePackageId txt of
    Nothing  -> fail ("not a package-id: " <> T.unpack txt)
    Just pid -> pure pid

pVersion :: (Char -> Bool) -> Parser Version
pVersion p = do
  txt <- A.takeTill p
  case parseVersion txt of
    Nothing  -> fail ("not a version number: " <> T.unpack txt)
    Just ver -> pure ver
