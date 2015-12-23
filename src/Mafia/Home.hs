{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
module Mafia.Home
  ( getMafiaHome
  , ensureMafiaDir
  , installBinary
  ) where

import           Control.Monad.IO.Class (MonadIO(..))

import           Data.Text (Text)
import qualified Data.Text as T

import           Mafia.IO
import           Mafia.Package
import           Mafia.Path
import           Mafia.Process

import           P

import           System.IO (IO)
import           System.IO.Temp (withTempDirectory)

import           X.Control.Monad.Trans.Either (EitherT, pattern EitherT, runEitherT)


getMafiaHome :: IO Text
getMafiaHome =
  (</> T.pack ".ambiata/mafia") <$> getHomeDirectory

ensureMafiaDir :: MonadIO m => Text -> m Text
ensureMafiaDir path = liftIO $ do
  home <- getMafiaHome
  let path' = home </> path
  createDirectoryIfMissing True path'
  pure path'

-- | Installs a given cabal package at a specific version
installBinary :: PackageId -> [PackageId] -> EitherT ProcessError IO File
installBinary package deps = do
  tmp <- ensureMafiaDir "tmp"
  let nv = renderPackageId package
  bin <- ensureMafiaDir "bin"
  let path = bin </> nv
  liftIO (doesFileExist path) >>= \case
    True ->
      pure path
    False -> do
      EitherT . withTempDirectory (T.unpack tmp) (T.unpack $ nv <> ".") $ \sandboxDir -> runEitherT $ do
        Pass <- callFrom id (T.pack sandboxDir) "cabal" ["sandbox", "init"]
        -- Install any required executables first
        -- This could also recursively call `installBinary` and copy the output in to the sandbox if we do this again
        for_ deps $ \p -> do
          Pass <- callFrom id (T.pack sandboxDir) "cabal" ["install", renderPackageId p]
          pure ()
        Pass <- callFrom id (T.pack sandboxDir) "cabal" ["install", nv]
        copyFile (T.pack sandboxDir </> ".cabal-sandbox" </> "bin" </> (unPackageName . pkgName) package) path
        return path
