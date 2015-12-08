#!/usr/bin/env runhaskell

import           Data.Time (formatTime, getCurrentTime)
import           Data.Time.Locale.Compat (defaultTimeLocale)
import           Data.List (intercalate)
import           Data.Version (showVersion)

import           Distribution.PackageDescription
import           Distribution.Verbosity
import           Distribution.Simple
import           Distribution.Simple.Setup (BuildFlags(..), ReplFlags(..), TestFlags(..), fromFlag)
import           Distribution.Simple.LocalBuildInfo
import           Distribution.Simple.BuildPaths (autogenModulesDir)
import           Distribution.Simple.Utils (createDirectoryIfMissingVerbose, rewriteFile)

import           System.FilePath ((</>), (<.>))
import           System.Process (readProcess)

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
       (buildHook hooks) pd lbi uh flags
   , replHook = \pd lbi uh flags args -> do
       genBuildInfo (fromFlag $ replVerbosity flags) pd
       (replHook hooks) pd lbi uh flags args
   , testHook = \args pd lbi uh flags -> do
       genBuildInfo (fromFlag $ testVerbosity flags) pd
       (testHook hooks) args pd lbi uh flags
   }

genBuildInfo :: Verbosity -> PackageDescription -> IO ()
genBuildInfo verbosity pkg = do
  createDirectoryIfMissingVerbose verbosity True "gen"
  let (PackageName pname) = pkgName . package $ pkg
      version = pkgVersion . package $ pkg
      name = "BuildInfo_" ++ (map (\c -> if c == '-' then '_' else c) pname)
      targetHs = "gen" </> name <.> "hs"
      targetText = "gen" </> "version.txt"
  t <- timestamp
  gv <- gitVersion
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

timestamp :: IO String
timestamp =
  formatTime defaultTimeLocale "%Y%m%d%H%M%S" `fmap` getCurrentTime

gitVersion :: IO String
gitVersion = do
  ver <- readProcess "git" ["log", "--pretty=format:%h", "-n", "1"] ""
  notModified <- ((>) 1 . length) `fmap` readProcess "git" ["status", "--porcelain"] ""
  return $ ver ++ if notModified then "" else "-M"
