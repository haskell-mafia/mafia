{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Mafia.Hoogle
  ( HooglePackagesSandbox (..)
  , HooglePackagesCached (..)
  , hoogle
  , joinHooglePackages
  ) where

import           Control.Monad.Trans.Bifunctor (firstT)
import           Control.Monad.Trans.Either (EitherT, hoistEither, runEitherT, left)

import qualified Data.List as L
import           Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T

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

data Hoogle =
  Hoogle {
      hooglePath :: File
    , _hoogleVersion :: HoogleVersion
    }

data HoogleVersion =
    Hoogle4x
  | Hoogle5x

hoogle :: Text -> [Argument] -> EitherT MafiaError IO ()
hoogle hackageRoot args = do
  hp <- hooglePackages hackageRoot
  hpc <- hooglePackagesCached
  hoogleIndex args $ joinHooglePackages hpc hp

-- | Download all packages installed in the local sandbox into a global location
hooglePackages :: Text -> EitherT MafiaError IO HooglePackagesSandbox
hooglePackages hackageRoot = do
  firstT MafiaInitError $ initialize LatestSources Nothing Nothing
  db <- hoogleCacheDir
  hoogleExe <- findHoogle
  Out pkgStr <- liftCabal $ cabal "exec" ["--", "ghc-pkg", "list", "--simple-output"]
  let pkgs = T.splitOn " " . T.strip $ pkgStr
  fmap (HooglePackagesSandbox . catMaybes) . for pkgs $ \pkg -> do
    pkgId <- hoistEither . maybeToRight (MafiaParseError $ mconcat ["Invalid package: ", pkg]) . parsePackageId $ pkg
    let name = unPackageName . pkgName $ pkgId
    let txt = db </> pkg <> ".txt"
    let hoo = hoogleDbFile' hoogleExe db pkgId
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
            case hoogleExe of
              Hoogle hoogleExe' Hoogle4x ->
                call_ MafiaProcessError hoogleExe' ["convert", txt]
              Hoogle _ Hoogle5x ->
                -- There isn't an associated hoogle 5.x command for this
                pure ()
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
  hoogleExe <- findHoogle
  db' <- (\d -> d </> "hoogle" </> hash) <$> liftCabal initSandbox
  case hoogleExe of
    Hoogle hoogleExe' Hoogle4x -> do
      unlessM (doesFileExist $ db' </> "default.hoo") $ do
        createDirectoryIfMissing True db'
        -- We may also want to copy/symlink all the hoo files here to allow for partial module searching
        call_ MafiaProcessError hoogleExe' $
          ["combine", "--outfile", db' </> "default.hoo"] <> fmap (hoogleDbFile db) pkgs
      call_ MafiaProcessError (hooglePath hoogleExe) $ ["-d", db'] <> args

    Hoogle hoogleExe' Hoogle5x -> do
      unlessM (doesFileExist $ db' </> "default.hoo") $ do
        createDirectoryIfMissing True db'

        -- Link each hoogle file into `db'` directory
        forM_ pkgs $ \pkg -> do
          let src = db </> renderPackageId pkg <> ".txt"
          let dst = db' </> takeFileName src
          createSymbolicLink src dst

        let a = mconcat $ [
              ["generate", "--database", db' </> "default.hoo"
              , "--local=" <> db']
              ]
        call_ MafiaProcessError hoogleExe' a
      call_ MafiaProcessError (hooglePath hoogleExe) $ ["-d", db' </> "default.hoo"] <> args

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
findHoogle :: EitherT MafiaError IO Hoogle
findHoogle = do
  h <- findHoogleExe
  v <- detectHoogleVersion h
  pure $ Hoogle h v

findHoogleExe :: EitherT MafiaError IO File
findHoogleExe = do
  res <- runEitherT $ T.init . unOut <$> call MafiaProcessError "which" ["hoogle"]
  case res of
    Right path -> pure path
    Left x ->
      -- TODO More friendly error messages about expecting to find `hoogle` on $PATH
      left . MafiaParseError $ ("Invalid hoogle version: " <> renderMafiaError x)

detectHoogleVersion :: File -> EitherT MafiaError IO HoogleVersion
detectHoogleVersion hf = do
  res <- T.init . unOut <$> call MafiaProcessError hf ["--version"]
  if T.isPrefixOf "Hoogle v4." res then
    pure Hoogle4x
  else if T.isPrefixOf "Hoogle 5." res then
    pure Hoogle5x
  else
    left . MafiaParseError $ "Invalid hoogle version: " <> res

hoogleDbFile :: Directory -> PackageId -> File
hoogleDbFile db pkg =
  db </> renderPackageId pkg <> ".hoo"

hoogleDbFile' :: Hoogle -> Directory -> PackageId -> File
hoogleDbFile' v db pkg = case v of
  Hoogle _ Hoogle4x ->
    db </> renderPackageId pkg <> ".hoo"
  Hoogle _ Hoogle5x ->
    db </> renderPackageId pkg <> ".txt"

mapFromListGrouped :: Ord a => [(a, b)] -> Map a [b]
mapFromListGrouped =
  foldr (\(k, v) -> M.insertWith (<>) k [v]) M.empty
