#!/usr/bin/env runhaskell

import           Data.Char (isDigit, toLower)
import           Data.Function (on)
import           Data.List (intercalate, sortBy)
import           Data.Monoid ((<>))
import           Data.Version (showVersion)

import           Distribution.InstalledPackageInfo
import           Distribution.PackageDescription
import           Distribution.Simple
import           Distribution.Simple.Setup (BuildFlags(..), ReplFlags(..), TestFlags(..), fromFlag)
import           Distribution.Simple.LocalBuildInfo
import           Distribution.Simple.PackageIndex
import           Distribution.Simple.BuildPaths (autogenModulesDir)
import           Distribution.Simple.Utils (createDirectoryIfMissingVerbose, rewriteFile, rawSystemStdout)
import           Distribution.Verbosity

main :: IO ()
main =
  let hooks = simpleUserHooks
   in defaultMainWithHooks hooks {
     preConf = \args flags -> do
       createDirectoryIfMissingVerbose silent True "gen"
       (preConf hooks) args flags
   , sDistHook  = \pd mlbi uh flags -> do
       genBuildInfo silent pd
       (sDistHook hooks) pd mlbi uh flags
   , buildHook = \pd lbi uh flags -> do
       genBuildInfo (fromFlag $ buildVerbosity flags) pd
       genDependencyInfo (fromFlag $ buildVerbosity flags) pd lbi
       (buildHook hooks) pd lbi uh flags
   , replHook = \pd lbi uh flags args -> do
       genBuildInfo (fromFlag $ replVerbosity flags) pd
       genDependencyInfo (fromFlag $ replVerbosity flags) pd lbi
       (replHook hooks) pd lbi uh flags args
   , testHook = \args pd lbi uh flags -> do
       genBuildInfo (fromFlag $ testVerbosity flags) pd
       genDependencyInfo (fromFlag $ testVerbosity flags) pd lbi
       (testHook hooks) args pd lbi uh flags
   }

genBuildInfo :: Verbosity -> PackageDescription -> IO ()
genBuildInfo verbosity pkg = do
  createDirectoryIfMissingVerbose verbosity True "gen"
  let (PackageName pname) = pkgName . package $ pkg
      version = pkgVersion . package $ pkg
      name = "BuildInfo_" ++ (map (\c -> if c == '-' then '_' else c) pname)
      targetHs = "gen/" ++ name ++ ".hs"
      targetText = "gen/version.txt"
  t <- timestamp verbosity
  gv <- gitVersion verbosity
  let v = showVersion version
  let buildVersion = intercalate "-" [v, t, gv]
  rewriteFile targetHs $ unlines [
      "module " ++ name ++ " where"
    , "import Prelude"
    , "data RuntimeBuildInfo = RuntimeBuildInfo { buildVersion :: String, timestamp :: String, gitVersion :: String }"
    , "buildInfo :: RuntimeBuildInfo"
    , "buildInfo = RuntimeBuildInfo \"" ++ v ++ "\" \"" ++ t ++ "\" \"" ++ gv ++ "\""
    , "buildInfoVersion :: String"
    , "buildInfoVersion = \"" ++ buildVersion ++ "\""
    ]
  rewriteFile targetText buildVersion

genDependencyInfo :: Verbosity -> PackageDescription -> LocalBuildInfo -> IO ()
genDependencyInfo verbosity pkg info = do
  let
    (PackageName pname) = pkgName . package $ pkg
    name = "DependencyInfo_" ++ (map (\c -> if c == '-' then '_' else c) pname)
    targetHs = autogenModulesDir info ++ "/" ++ name ++ ".hs"
    render p =
      let
        n = unPackageName $ pkgName p
        v = intercalate "." . fmap show . versionBranch $ pkgVersion p
      in
       n ++ "-" ++ v
    deps = fmap (render . sourcePackageId) . allPackages $ installedPkgs info
    sdeps = sortBy (compare `on` fmap toLower) deps
    strs = flip fmap sdeps $ \d -> "\"" ++ d ++ "\""

  createDirectoryIfMissingVerbose verbosity True (autogenModulesDir info)

  rewriteFile targetHs $ unlines [
      "module " ++ name ++ " where"
    , "import Prelude"
    , "dependencyInfo :: [String]"
    , "dependencyInfo = [\n    " ++ intercalate "\n  , " strs ++ "\n  ]"
    ]

gitVersion :: Verbosity -> IO String
gitVersion verbosity = do
  ver <- rawSystemStdout verbosity "git" ["log", "--pretty=format:%h", "-n", "1"]
  notModified <- ((>) 1 . length) `fmap` rawSystemStdout verbosity "git" ["status", "--porcelain"]
  return $ ver ++ if notModified then "" else "-M"

timestamp :: Verbosity -> IO String
timestamp verbosity =
  rawSystemStdout verbosity "date" ["+%Y%m%d%H%M%S"] >>= \s ->
    case splitAt 14 s of
      (d, n : []) ->
        if (length d == 14 && filter isDigit d == d)
          then return d
          else fail $ "date has failed to produce the correct format [" <> s <> "]."
      _ ->
        fail $ "date has failed to produce a date long enough [" <> s <> "]."
