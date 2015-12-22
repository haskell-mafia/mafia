{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Mafia.Hoogle
  ( hoogle
  ) where

import           Control.Monad.IO.Class (MonadIO(..))

import qualified Crypto.Hash as Hash

import qualified Data.List as List
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T

import           Mafia.Cache
import           Mafia.Error
import           Mafia.Home
import           Mafia.IO
import           Mafia.Path
import           Mafia.Process
import           Mafia.Sandbox

import           P

import           System.IO (IO, stderr)

import           X.Control.Monad.Trans.Either (EitherT, firstEitherT, runEitherT)


hoogle :: Text -> [Argument] -> EitherT MafiaViolation IO ()
hoogle hackageRoot args = do
  initialize
  db <- ensureMafiaDir "hoogle"
  hoogleExe <- firstEitherT ProcessError $ installBinary "hoogle" "4.2.43" [("happy", "1.19.5")]
  Out pkgStr <- sandbox "hc-pkg" ["list"]
  let pkgs = fmap T.strip . filter (T.isPrefixOf " ") . T.lines $ pkgStr
  hoos <- fmap catMaybes . for pkgs $ \pkg -> do
    -- Extract name from `$name-$version`, but consider `unordered-containers-1.2.3`
    let name = T.intercalate "-" . List.init $ T.splitOn "-" pkg
    let txt = db </> pkg <> ".txt"
    let hoo = db </> pkg <> ".hoo"
    let skip = db </> pkg <> ".skip"
    ifM (doesFileExist skip) (pure Nothing) $
      ifM (doesFileExist hoo) (pure $ Just hoo) $ do
        liftIO . T.hPutStrLn stderr $ "Downloading: " <> pkg
        r <- runEitherT $ call ProcessError "curl" ["-f", "-s", hackageRoot </> pkg </> "docs" </> name <> ".txt", "-o", txt]
        case r of
          Left _ -> do
            liftIO . T.hPutStrLn stderr $ "Missing: " <> pkg
            -- Technically we can "convert" a broken txt file and no one is the wiser, but we're not going to do that
            liftIO $ T.writeFile (T.unpack skip) ""
            pure Nothing
          Right Hush -> do
            call_ ProcessError hoogleExe ["convert", txt]
            pure $ Just hoo
  -- By default hoogle will expect a 'default.hoo' file to exist in the database directory
  -- If we want the search to just be for _this_ sandbox, we have two options
  -- 1. Create a unique directory based on all the current packages and ensure the default.hoo
  -- 2. Specify/append all the packages from the global database by using "+$name-$version"
  --    Unfortunately hoogle doesn't like the "-$version" part :(
  let hash = T.decodeUtf8 . (\(d :: Hash.Digest Hash.SHA1) -> Hash.digestToHexByteString d) . Hash.hash . T.encodeUtf8 . mconcat $ pkgs
  db' <- (\d -> d </> "hoogle" </> hash) <$> initSandbox
  unlessM (doesFileExist $ db' </> "default.hoo") $ do
    createDirectoryIfMissing True db'
    -- We may also want to copy/symlink all the hoo files here to allow for partial module searching
    call_ ProcessError hoogleExe $ ["combine", "--outfile", db' </> "default.hoo"] <> hoos
  call_ ProcessError hoogleExe $ ["-d", db'] <> args
