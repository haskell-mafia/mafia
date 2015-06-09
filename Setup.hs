#!/usr/bin/env runhaskell

import           Data.Time (formatTime, getCurrentTime)
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
import           System.Locale (defaultTimeLocale)
import           System.Process (readProcess)

main :: IO ()
main =
  let hooks = simpleUserHooks
   in defaultMainWithHooks hooks {
     buildHook = \pd lbi uh flags -> do
       genBuildInfo (fromFlag $ buildVerbosity flags) pd lbi
       (buildHook hooks) pd lbi uh flags
   , replHook = \pd lbi uh flags args -> do
       genBuildInfo (fromFlag $ replVerbosity flags) pd lbi
       (replHook hooks) pd lbi uh flags args
   , testHook = \args pd lbi uh flags -> do
       genBuildInfo (fromFlag $ testVerbosity flags) pd lbi
       (testHook hooks) args pd lbi uh flags
   }

genBuildInfo :: Verbosity -> PackageDescription -> LocalBuildInfo -> IO ()
genBuildInfo verbosity pkg info = do
  createDirectoryIfMissingVerbose verbosity True (autogenModulesDir info)
  let (PackageName pname) = pkgName . package $ pkg
      version = pkgVersion . package $ pkg
      name = "BuildInfo_" ++ (map (\c -> if c == '-' then '_' else c) pname)
      targetHs = autogenModulesDir info </> name <.> "hs"
      targetText = autogenModulesDir info </> "version.txt"
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
