{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Mafia.Hoogle
  ( HooglePackagesSandbox (..)
  , HooglePackagesCached (..)
  , hoogle
  , joinHooglePackages
  ) where

import           Control.Monad.Trans.Bifunctor (firstT, bimapT)
import           Control.Monad.Trans.Either (EitherT, hoistEither, runEitherT)

import qualified Data.List as L
import           Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Mafia.Bin
import           Mafia.Cabal
import           Mafia.Error
import           Mafia.Hash
import           Mafia.Home
import           Mafia.IO
import           Mafia.Init
import           Mafia.Package
import           Mafia.Path
import           Mafia.Process
import           Mafia.P

import           System.IO (IO, stderr)

newtype HooglePackagesSandbox = HooglePackagesSandbox [PackageId]
newtype HooglePackagesCached = HooglePackagesCached [PackageId]


hoogle :: Text -> [Argument] -> EitherT MafiaError IO ()
hoogle hackageRoot args = do
  hp <- hooglePackages hackageRoot
  hpc <- hooglePackagesCached
  hoogleIndex args $ joinHooglePackages hpc hp

hooglePackages :: Text -> EitherT MafiaError IO HooglePackagesSandbox
hooglePackages hackageRoot = do
  firstT MafiaInitError $ initialize LatestSources Nothing Nothing
  db <- hoogleCacheDir
  hoogleExe <- findHoogleExe
  Out pkgStr <- liftCabal $ cabal "exec" ["--", "ghc-pkg", "list", "--simple-output"]
  let pkgs = T.splitOn " " . T.strip $ pkgStr
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
  let hash = renderHash .  hashText .  mconcat .  fmap renderPackageId $ pkgs
  db <- hoogleCacheDir
  hoogleExe <- findHoogleExe
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

-- | Find the 'hoogle' executable on $PATH and it if isn't there, install it.
findHoogleExe :: EitherT MafiaError IO File
findHoogleExe = do
  res <- runEitherT $ T.init . unOut <$> call MafiaProcessError "which" ["hoogle"]
  case res of
    Right path -> pure path
    Left _ -> installHoogle

installHoogle :: EitherT MafiaError IO File
installHoogle =
  bimapT MafiaBinError (</> "hoogle") $ do
    installBinary (ipackageId "hoogle" [4, 2, 43]) [
        -- Hoogle can't build with the shake >= 0.16
        ConstraintBounded (mkPackageName "shake") (Exclusive (makeVersion [0])) (Just (Exclusive (makeVersion [0, 16])))
      ]

hoogleDbFile :: Directory -> PackageId -> File
hoogleDbFile db pkg =
  db </> renderPackageId pkg <> ".hoo"

mapFromListGrouped :: Ord a => [(a, b)] -> Map a [b]
mapFromListGrouped =
  foldr (\(k, v) -> M.insertWith (<>) k [v]) M.empty
