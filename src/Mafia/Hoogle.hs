{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Mafia.Hoogle
  ( HooglePackagesSandbox (..)
  , HooglePackagesCached (..)
  , hoogle
  , joinHooglePackages
  ) where

import           Control.Monad.IO.Class (MonadIO(..))

import qualified Crypto.Hash as Hash

import qualified Data.List as L
import           Data.Map (Map)
import qualified Data.Map as M
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

import           X.Control.Monad.Trans.Either (EitherT, hoistEither, runEitherT)


newtype HooglePackagesSandbox = HooglePackagesSandbox [PackageId]
newtype HooglePackagesCached = HooglePackagesCached [PackageId]


hoogle :: Text -> [Argument] -> EitherT MafiaError IO ()
hoogle hackageRoot args = do
  hp <- hooglePackages hackageRoot
  hpc <- hooglePackagesCached
  hoogleIndex args $ joinHooglePackages hpc hp

hooglePackages :: Text -> EitherT MafiaError IO HooglePackagesSandbox
hooglePackages hackageRoot = do
  firstT MafiaInitError $ initialize Nothing
  db <- hoogleCacheDir
  hoogleExe <- installHoogle
  Out pkgStr <- liftCabal $ sandbox "hc-pkg" ["list"]
  let pkgs = fmap (T.dropWhileEnd (== ')') . T.dropWhile (== '(') . T.strip) . filter (T.isPrefixOf " ") . T.lines $ pkgStr
  fmap (HooglePackagesSandbox . catMaybes) . for pkgs $ \pkg -> do
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

hooglePackagesCached :: (Functor m, MonadIO m) => m HooglePackagesCached
hooglePackagesCached = do
  db <- hoogleCacheDir
  HooglePackagesCached . catMaybes . fmap ((=<<) parsePackageId . T.stripSuffix ".hoo" . takeFileName) <$>
    getDirectoryListing (RecursiveDepth 1) db

-- | Keep everything from the current sandbox and append the latest of any remaining packages
joinHooglePackages :: HooglePackagesCached -> HooglePackagesSandbox -> [PackageId]
joinHooglePackages (HooglePackagesCached cached) (HooglePackagesSandbox current) =
  let index = mapFromListGrouped . fmap packageIdTuple
      extra = fmap (uncurry PackageId) . M.toList . M.mapMaybe (head . reverse . L.sort) $ M.difference (index cached) (index current)
  in current <> extra

hoogleCacheDir :: MonadIO m => m Directory
hoogleCacheDir =
  ensureMafiaDir "hoogle"

installHoogle :: EitherT MafiaError IO File
installHoogle =
  bimapT MafiaProcessError (</> "hoogle") $
    installBinary (packageId "hoogle" [4, 2, 43]) [packageId "happy" [1, 19, 5]]

hoogleDbFile :: Directory -> PackageId -> File
hoogleDbFile db pkg =
  db </> renderPackageId pkg <> ".hoo"

mapFromListGrouped :: Ord a => [(a, b)] -> Map a [b]
mapFromListGrouped =
  foldr (\(k, v) -> M.insertWith (<>) k [v]) M.empty
