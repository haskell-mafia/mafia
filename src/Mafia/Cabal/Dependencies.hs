{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
module Mafia.Cabal.Dependencies
  ( findDependencies
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
  fromRevDeps spkgs <$> findRevDeps (fmap spDirectory spkgs)

fromRevDeps :: [SourcePackage] -> [RevDeps] -> [Package]
fromRevDeps spkgs rdeps =
  let rdMap =
        mapFromList (refId . revRef) rdeps

      spCombine s r =
        r { revRef = (revRef r) { refSrcPkg = Just s } }

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

findRevDeps :: [Directory] -> EitherT CabalError IO [RevDeps]
findRevDeps sdirs = do
  EitherT . withSystemTempDirectory "mafia-deps-" $ \tmp0 -> runEitherT $ do
    dir <- getCurrentDirectory

    let tmp   = T.pack tmp0
        cabal = cabalFrom dir (Just (tmp </> "sandbox.config"))

    Hush <- cabal "sandbox" ["init", "--sandbox", tmp]

    -- this is a fast 'cabal sandbox add-source'
    createIndexFile sdirs tmp

    let installDryRun args =
          cabal "install" $
            [ "--only-dependencies"
            , "--force-reinstalls"
            , "--enable-tests"
            , "--enable-benchmarks"
            , "--reorder-goals"
            , "--max-backjumps=-1"
            , "--dry-run" ] <> args

    result <- liftIO . runEitherT $ installDryRun ["-v2"]
    case result of
      Right (OutErr out _) ->
        hoistEither (parseRevDeps out)
      Left _ -> do
        -- this will fail with the standard cabal dependency error message
        Pass <- installDryRun []
        -- this should never happen
        left CabalInstallIsNotReferentiallyTransparent

mapFromList :: Ord k => (v -> k) -> [v] -> Map k v
mapFromList f xs = Map.fromList (List.zip (fmap f xs) xs)

toGraphKey :: RevDeps -> (RevDeps, PackageId, [PackageId])
toGraphKey rev = (rev, refId (revRef rev), revDeps rev)

fromGraphKey :: (RevDeps, PackageId, [PackageId]) -> PackageRef
fromGraphKey (rev, _, _) = revRef rev

------------------------------------------------------------------------

data RevDeps =
  RevDeps {
      revRef  :: PackageRef
    , revDeps :: [PackageId]
    } deriving (Eq, Ord, Show)

parseRevDeps :: Text -> Either CabalError [RevDeps]
parseRevDeps =
  first (CabalParseError . T.pack) .
  traverse parseDep .
  List.drop 1 .
  List.dropWhile (/= "In order, the following would be installed:") .
  T.lines

parseDep :: Text -> Either String RevDeps
parseDep txt =
  let go err = "Invalid dependency line: " <> T.unpack txt <> "\nExpected: " <> err
  in first go (A.parseOnly pRevDeps txt)

pRevDeps :: Parser RevDeps
pRevDeps = do
  pid  <- pPackageId (== ' ') <* A.skipSpace
  _    <- optional pLatest
  fs   <- many pFlag
  deps <- fromMaybe [] <$> optional pVia
  _    <- optional (pNewPackage <|> pNewVersion)
  ()   <- A.endOfInput
  pure (RevDeps (PackageRef pid fs Nothing) deps)

pPackageId :: (Char -> Bool) -> Parser PackageId
pPackageId p = do
  txt <- A.takeTill p
  case parsePackageId txt of
    Nothing  -> fail ("not a package-id: " <> T.unpack txt)
    Just pid -> pure pid

pFlag :: Parser Flag
pFlag =
  let flag p = A.char p *> A.takeTill (== ' ') <* A.skipSpace
  in FlagOff <$> flag '-' <|>
     FlagOn  <$> flag '+'

pLatest :: Parser Text
pLatest =
  A.string "(latest: " *> A.takeTill (== ')') <* A.char ')' <* A.skipSpace

pVia :: Parser [PackageId]
pVia = do
  let pkg = pPackageId (\x -> x == ' ' || x == ')')
  A.string "(via: " *> pkg `A.sepBy1` A.char ' ' <* A.char ')' <* A.skipSpace

pNewPackage :: Parser ()
pNewPackage =
  A.string "(new package)" *> pure () <* A.skipSpace

pNewVersion :: Parser ()
pNewVersion =
  A.string "(new version)" *> pure () <* A.skipSpace
