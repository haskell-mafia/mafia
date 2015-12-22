{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Mafia.Hoogle
  ( hoogle
  ) where

import           Control.Monad.IO.Class (MonadIO(..))

import qualified Crypto.Hash as Hash

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T

import           Mafia.Cabal
import           Mafia.Error
import           Mafia.Home
import           Mafia.IO
import           Mafia.Init
import           Mafia.Package
import           Mafia.Path
import           Mafia.Process

import           P

import           System.IO (IO, stderr)

import           X.Control.Monad.Trans.Either (EitherT, firstEitherT, hoistEither, runEitherT)


hoogle :: Text -> [Argument] -> EitherT MafiaError IO ()
hoogle hackageRoot args =
  hooglePackages hackageRoot >>= hoogleIndex args

hooglePackages :: Text -> EitherT MafiaError IO [PackageId]
hooglePackages hackageRoot = do
  initialize
  db <- hoogleCacheDir
  hoogleExe <- installHoogle
  Out pkgStr <- liftCabal $ sandbox "hc-pkg" ["list"]
  let pkgs = fmap (T.dropWhileEnd (== ')') . T.dropWhile (== '(') . T.strip) . filter (T.isPrefixOf " ") . T.lines $ pkgStr
  fmap catMaybes . for pkgs $ \pkg -> do
    pkgId <- hoistEither . maybeToRight (MafiaParseError $ mconcat ["Invalid package: ", pkg]) . parsePackageId $ pkg
    let name = unPackageName . pkgName $ pkgId
    let txt = db </> pkg <> ".txt"
    let hoo = hoogleDbFile db pkgId
    let skip = db </> pkg <> ".skip"
    ifM (doesFileExist skip) (pure Nothing) $
      ifM (doesFileExist hoo) (pure $ Just pkgId) $ do
        liftIO . T.hPutStrLn stderr $ "Downloading: " <> pkg
        r <- runEitherT $ call MafiaProcessError "curl" ["-f", "-s", hackageRoot </> pkg </> "docs" </> name <> ".txt", "-o", txt]
        case r of
          Left _ -> do
            liftIO . T.hPutStrLn stderr $ "Missing: " <> pkg
            -- Technically we can "convert" a broken txt file and no one is the wiser, but we're not going to do that
            liftIO $ T.writeFile (T.unpack skip) ""
            pure Nothing
          Right Hush -> do
            call_ MafiaProcessError hoogleExe ["convert", txt]
            pure $ Just pkgId

hoogleIndex :: [Argument] -> [PackageId] -> EitherT MafiaError IO ()
hoogleIndex args pkgs = do
  -- By default hoogle will expect a 'default.hoo' file to exist in the database directory
  -- If we want the search to just be for _this_ sandbox, we have two options
  -- 1. Create a unique directory based on all the current packages and ensure the default.hoo
  -- 2. Specify/append all the packages from the global database by using "+$name-$version"
  --    Unfortunately hoogle doesn't like the "-$version" part :(
  let hash = T.decodeUtf8 . (\(d :: Hash.Digest Hash.SHA1) -> Hash.digestToHexByteString d) . Hash.hash . T.encodeUtf8 .
        mconcat . fmap renderPackageId $ pkgs
  db <- hoogleCacheDir
  hoogleExe <- installHoogle
  db' <- (\d -> d </> "hoogle" </> hash) <$> liftCabal initSandbox
  unlessM (doesFileExist $ db' </> "default.hoo") $ do
    createDirectoryIfMissing True db'
    -- We may also want to copy/symlink all the hoo files here to allow for partial module searching
    call_ MafiaProcessError hoogleExe $ ["combine", "--outfile", db' </> "default.hoo"] <> fmap (hoogleDbFile db) pkgs
  call_ MafiaProcessError hoogleExe $ ["-d", db'] <> args

hoogleCacheDir :: MonadIO m => m Directory
hoogleCacheDir =
  ensureMafiaDir "hoogle"

installHoogle :: EitherT MafiaError IO File
installHoogle =
  firstEitherT MafiaProcessError $ installBinary (packageId "hoogle" [4, 2, 43]) [packageId "happy" [1, 19, 5]]

hoogleDbFile :: Directory -> PackageId -> File
hoogleDbFile db pkg =
  db </> renderPackageId pkg <> ".hoo"
