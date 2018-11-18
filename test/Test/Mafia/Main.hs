module Test.Mafia.Main (
    disorderMain
  , disorderCliMain
  ) where

import           Control.Applicative
import           Control.Monad

import           System.Directory
import           System.Process
import           System.Exit
import           System.IO

import           Prelude

disorderMain :: [IO Bool] -> IO ()
disorderMain tests =
  sanity >> sequence tests >>= \rs -> unless (and rs) exitFailure

disorderCliMain :: [String] -> IO ()
disorderCliMain arguments =
  let ignore p = ".." == p || "." == p || "core" == p
      exec t = callProcess ("test/cli/" ++ t ++ "/run") arguments
   in sanity >> filter (not . ignore) <$> getDirectoryContents "test/cli/" >>= mapM_ exec

sanity :: IO ()
sanity =
  hSetBuffering stdout LineBuffering >> hSetBuffering stderr LineBuffering
