{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Mafia.Chaos where

import           Control.Exception (IOException)
import           Control.Monad.Catch
import           Control.Monad.IO.Class (MonadIO(..))

import           Disorder.Corpus
import           Disorder.Core.IO

import           Data.Char (toUpper)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Mafia.IO
import           Mafia.Path
import           Mafia.Process

import           P

import           System.IO (IO, print)
import           System.IO.Temp (withSystemTempDirectory)

import           Test.QuickCheck

import           X.Control.Monad.Trans.Either

------------------------------------------------------------------------

data Action =
    Build
  | AddLib    Text
  | RemoveLib Text
  deriving (Eq, Ord, Show)

instance Arbitrary Action where
  arbitrary = oneof [ AddLib    <$> genPackageName
                    , RemoveLib <$> genPackageName
                    , pure Build ]

genPackageName :: Gen Text
genPackageName = T.replace " " "-" <$> elements viruses

------------------------------------------------------------------------

bracketDirectory :: IO a -> IO a
bracketDirectory io = bracket getCurrentDirectory setCurrentDirectory (const io)

withTempDirectory :: Testable a => (Directory -> EitherT ProcessError IO a) -> Property
withTempDirectory io = testIO . bracketDirectory $ do
  result  <- withSystemTempDirectory "mafia.chaos" (runEitherT . io . T.pack)
  case result of
    Left  err -> return (counterexample (show err) False)
    Right x   -> return (property x)

hush :: (MonadIO m, MonadCatch m) => a -> m a -> m a
hush def = handle $ \(_ :: IOException) -> return def

------------------------------------------------------------------------

writeRootProject :: (MonadIO m, MonadCatch m) => m ()
writeRootProject = do
  libs <- hush [] $ getDirectoryContents "lib"
  writeProject "." "root" libs

writeProject :: MonadIO m => Directory -> Text -> [Text] -> m ()
writeProject dir name deps = do
  let src     = dir </> "src"
      modName = dromedary name

  writeFile (dir </> name <> ".cabal")
            (cabalText name deps)

  writeFile (src </> modName <> ".hs")
            ("module " <> modName <> " where\n")

writeFile :: MonadIO m => Path -> Text -> m ()
writeFile path txt = do
  createDirectoryIfMissing True (takeDirectory path)
  writeText path txt

dromedary :: Text -> Text
dromedary = mconcat . fmap upcase . T.splitOn "-"
  where
    upcase xs | T.null xs = xs
              | otherwise = T.singleton (toUpper (T.head xs)) <> T.tail xs

cabalText :: Text -> [Text] -> Text
cabalText name deps = T.unlines [
    "name:          acme-" <> name
  , "version:       0.0.1"
  , "license:       AllRightsReserved"
  , "author:        Mafia Chaos"
  , "maintainer:    Mafia Chaos"
  , "synopsis:      synopsis"
  , "category:      Development"
  , "cabal-version: >= 1.8"
  , "build-type:    Simple"
  , "description:   description"
  , ""
  , "library"
  , "  hs-source-dirs: src"
  , "  build-depends:"
  , "      base >= 3 && < 5"
  ] <> T.unlines (fmap ("    , acme-" <>) deps)

------------------------------------------------------------------------

prop_chaos (actions :: [Action]) = withTempDirectory $ \temp -> do
  liftIO (print actions)

  mafia <- (</> "dist/build/mafia/mafia") <$> getCurrentDirectory

  setCurrentDirectory temp
  call_ id "git" ["init"]
  writeRootProject

  forM_ actions $ \action -> do
    runAction mafia action

  return True

runAction :: File -> Action -> EitherT ProcessError IO ()
runAction mafia = \case
  AddLib lib -> do
    let dir = "lib" </> lib
    liftIO (T.putStrLn ("+ " <> dir))

    writeProject dir lib []
    writeRootProject

  RemoveLib lib -> do
    let dir = "lib" </> lib
    liftIO (T.putStrLn ("- " <> dir))

    hush () $ removeDirectoryRecursive dir
    writeRootProject

  Build -> do
    liftIO (T.putStrLn "$ mafia build")
    call_ id mafia ["build"]

------------------------------------------------------------------------

return []
tests = $forAllProperties $ quickCheckWithResult (stdArgs {maxSuccess = 10})
