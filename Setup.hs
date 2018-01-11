{-# LANGUAGE CPP #-}

import           Data.Char (isDigit)
import           Data.List (intercalate)
import           Data.Monoid ((<>))

import           Distribution.InstalledPackageInfo
import           Distribution.PackageDescription
import           Distribution.Simple (buildHook, defaultMainWithHooks, pkgName, pkgVersion, preConf, replHook, sDistHook, simpleUserHooks, testHook)
import           Distribution.Simple.Setup (BuildFlags(..), ReplFlags(..), TestFlags(..), fromFlag)
import           Distribution.Simple.LocalBuildInfo
import           Distribution.Simple.PackageIndex
import           Distribution.Simple.Utils (createDirectoryIfMissingVerbose, rewriteFile, rawSystemStdout)
import           Distribution.Verbosity

#if __GLASGOW_HASKELL__ <= 710
-- GHC 7.10 and earlier do not support the MIN_VERSION_Cabal macro.
-- Set it to `1` here to match the Cabal dependency bounds in the cabal file.
#define MIN_VERSION_Cabal(a,b,c) 1
#endif

#if MIN_VERSION_Cabal(2,0,0)
import           Distribution.Types.PackageName (PackageName, unPackageName)
import           Distribution.Simple.BuildPaths (autogenPackageModulesDir)
import           Distribution.Version (Version, versionNumbers)

showVersion :: Version -> String
showVersion = intercalate "." . fmap show . versionNumbers

autogenModulesDirCompat :: LocalBuildInfo -> String
autogenModulesDirCompat = autogenPackageModulesDir

#else
import           Distribution.Simple (PackageName, unPackageName)
import           Distribution.Simple.BuildPaths (autogenModulesDir)
import           Data.Version (showVersion)

autogenModulesDirCompat :: LocalBuildInfo -> String
autogenModulesDirCompat = autogenModulesDir
#endif


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
  let pname = unPackageName . pkgName . package $ pkg
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
    pname = unPackageName . pkgName . package $ pkg
    name = "DependencyInfo_" ++ (map (\c -> if c == '-' then '_' else c) pname)
    targetHs = autogenModulesDirCompat info ++ "/" ++ name ++ ".hs"
    render p =
      let
        n = unPackageName $ pkgName p
        v = showVersion $ pkgVersion p
      in
       n ++ "-" ++ v
    deps = fmap (render . sourcePackageId) . allPackages $ installedPkgs info
    strs = flip fmap deps $ \d -> "\"" ++ d ++ "\""

  createDirectoryIfMissingVerbose verbosity True (autogenModulesDirCompat info)

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
