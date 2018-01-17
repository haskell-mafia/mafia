{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Mafia.Script (
    ScriptError(..)
  , renderScriptError
  , runScript

  -- For testing.
  , Pragma (..)
  , Submodule (..)
  , SubmoduleLocation (..)
  , SubmoduleProtocol (..)
  , pSubmodule
  , renderPragmaSubmodule
  ) where

import qualified Data.Attoparsec.Text as Atto
import           Data.Char (isSpace)
import qualified Data.Map as Map
import qualified Data.Text as T

import           Mafia.Hash
import           Mafia.Home
import           Mafia.IO
import           Mafia.Path
import           Mafia.Process

import           Mafia.P

import           System.IO (IO)

import           Control.Monad.Trans.Either (EitherT, hoistMaybe, hoistEither)


data Script =
  Script {
      scriptId :: !Text
    , scriptHash :: !Hash
    , scriptPath :: !File
    , scriptPackages :: ![Package]
    , scriptSubmodules :: ![Submodule]
    , scriptMain :: !Text
    } deriving (Eq, Ord, Show)

newtype Package =
  Package {
      unPackage :: Text
    } deriving (Eq, Ord, Show)

data SubmoduleProtocol =
    Https
  | Git
    deriving (Eq, Ord, Show)

data Submodule =
  Submodule {
      submoduleLocation :: !(Maybe SubmoduleLocation)
    , submoduleUser :: !Text
    , submoduleRepo :: !Text
    , submoduleCommit :: !(Maybe Text)
    } deriving (Eq, Ord, Show)

data SubmoduleLocation =
  SubmoduleLocation {
      submoduleProtocol :: !SubmoduleProtocol
    , submoduleService :: !Text
    } deriving (Eq, Ord, Show)

data Pragma =
    PragmaPackage !Package
  | PragmaSubmodule !Submodule
    deriving (Eq, Ord, Show)

data ScriptError =
    ScriptProcessError !ProcessError
  | ScriptFileNotFound !File
  | ScriptGitParseError ![Char]
    deriving (Show)

renderScriptError :: ScriptError -> Text
renderScriptError = \case
  ScriptProcessError err ->
    renderProcessError err

  ScriptFileNotFound path ->
    "File not found: " <> path

  ScriptGitParseError err ->
    "Failed parsing git output: " <> T.pack err

pPragma :: Text -> Atto.Parser a -> Atto.Parser a
pPragma pragma parser = do
  _ <- Atto.string "{-#" *> Atto.skipSpace *> Atto.string pragma
  x <- Atto.skipSpace *> parser <* Atto.skipSpace
  _ <- Atto.string "#-}"
  pure x

pPackage :: Atto.Parser Pragma
pPackage =
  fmap PragmaPackage . pPragma "PACKAGE" $
    Package . T.strip <$> Atto.takeWhile (/= '#')

pNameChar :: Atto.Parser Text
pNameChar =
  Atto.takeWhile1 (Atto.inClass "a-zA-Z0-9-")

pHash :: Atto.Parser Text
pHash =
  Atto.takeWhile1 (Atto.inClass "a-fA-F0-9")

pSubmodule :: Atto.Parser Pragma
pSubmodule =
  fmap PragmaSubmodule . pPragma "SUBMODULE" $
    Submodule
      <$> Atto.option Nothing (Just <$> Atto.try pSubmoduleLocation)
      <*> pNameChar <* Atto.char '/'
      <*> pNameChar
      <*> optional (Atto.char '@' *> pHash)

pSubmoduleLocation :: Atto.Parser SubmoduleLocation
pSubmoduleLocation =
  SubmoduleLocation
    <$> ((Atto.string "git@" *> pure Git) <|> (Atto.string "https://" *> pure Https))
    <*> Atto.takeWhile1 (not . isSpace) <* Atto.takeWhile1 isSpace

tryParsePragma :: Text -> Maybe Pragma
tryParsePragma =
  rightToMaybe . Atto.parseOnly (Atto.try pPackage <|> pSubmodule)

takePackage :: Pragma -> Maybe Package
takePackage = \case
  PragmaPackage x ->
    Just x
  _ ->
    Nothing

takeSubmodule :: Pragma -> Maybe Submodule
takeSubmodule = \case
  PragmaSubmodule x ->
    Just x
  _ ->
    Nothing

parseScript :: File -> Text -> Script
parseScript path source =
  let
    pragmas =
      mapMaybe tryParsePragma $
      T.lines source

    packages =
      mapMaybe takePackage pragmas

    submodules =
      mapMaybe takeSubmodule pragmas

    sid =
      renderHash $ hashText path
  in
    Script sid (hashText source) path packages submodules source

-- | Render a full git url. If the SubmoduleLocation isn't given, it is assumed
-- to be 'git@github.com:'
renderSubmoduleUrl :: Submodule -> Text
renderSubmoduleUrl s =
  let
    url =
      case submoduleLocation s of
        Nothing -> "git@github.com:"
        Just l ->
          case submoduleProtocol l of
            Https ->  "https://" <> submoduleService l <> "/"
            Git ->  "git@" <> submoduleService l <> ":"
  in
    url <> submoduleUser s <> "/" <> submoduleRepo s

-- | Render a mafia script 'SUBMODULE' line. This is mainly for testing.
renderPragmaSubmodule :: Submodule -> Text
renderPragmaSubmodule s =
  let
    location =
      case submoduleLocation s of
        Nothing -> ""
        Just l ->
          case submoduleProtocol l of
            Https ->  "https://" <> submoduleService l
            Git ->  "git@" <> submoduleService l
    commit =
      case submoduleCommit s of
        Nothing -> ""
        Just c -> "@" <> c
  in
    "{-# SUBMODULE " <> location <> " " <> submoduleUser s <> "/" <> submoduleRepo s <> commit <> " #-}"

submoduleName :: Submodule -> Text
submoduleName s =
  "lib/" <> submoduleUser s <> "/" <> submoduleRepo s

getScriptHome :: MonadIO m => m Directory
getScriptHome =
  liftM (</> "scripts") getMafiaHome

getScriptDirectory :: MonadIO m => Script -> m Directory
getScriptDirectory script =
  liftM (</> scriptId script) getScriptHome

cabalText :: [Package] -> Text
cabalText deps =
  T.unlines $ [
      "name:          script"
    , "version:       0.0.1"
    , "license:       AllRightsReserved"
    , "author:        script"
    , "maintainer:    script"
    , "synopsis:      script"
    , "category:      Tools"
    , "cabal-version: >= 1.8"
    , "build-type:    Simple"
    , "description:   script"
    , ""
    , "executable script"
    , "  main-is:"
    , "    script.hs"
    , ""
    , "  ghc-options:"
    , "    -fno-warn-unrecognised-pragmas"
    , ""
    , "  build-depends:"
    , "      base"
    ] <> fmap (("    , " <>) . unPackage) deps

readHash :: MonadIO m => File -> m (Maybe Hash)
readHash file = do
  mtxt <- readUtf8 file
  return $ mtxt >>= parseHash

writeChanged :: MonadIO m => File -> Text -> m ()
writeChanged file txt = do
  mtxt <- readUtf8 file
  unless (Just txt == mtxt) $ do
    writeUtf8 file txt

pGitSubmodule :: Atto.Parser Submodule
pGitSubmodule = do
  _ <- Atto.char ' ' -- should never be '-' or '+' given we're managing the directory
  commit <- Atto.takeWhile (Atto.inClass "0-9a-f") <* Atto.char ' '
  _ <- Atto.string "lib/"
  user <- pNameChar <* Atto.char '/'
  repo <- pNameChar <* Atto.char ' '
  _ <- Atto.char '(' *> Atto.takeWhile (/= ')') <* Atto.char ')' <* Atto.endOfLine
  pure $ Submodule Nothing user repo (Just commit)

parseGitSubmodules :: Text -> Either ScriptError [Submodule]
parseGitSubmodules =
  first ScriptGitParseError . Atto.parseOnly (many pGitSubmodule <* Atto.endOfInput)

getSubmodules :: Directory -> EitherT ScriptError IO [Submodule]
getSubmodules dir = do
   Out xs <- callFrom ScriptProcessError dir "git" ["submodule"]
   hoistEither $ parseGitSubmodules xs

data SubmoduleAction =
    Add !Submodule
  | Remove !Submodule
  | Update !Submodule
    deriving (Eq, Ord, Show)

diffSubmodules :: [Submodule] -> [Submodule] -> [SubmoduleAction]
diffSubmodules current0 desired0 =
  let
    mkMap =
      Map.fromList .
      fmap (\x -> (submoduleName x, x))

    current =
      mkMap current0

    desired =
      mkMap desired0

    update _key d c =
      case (submoduleCommit d, submoduleCommit c) of
        (Nothing, Nothing) ->
          Nothing
        (Nothing, Just _) ->
          Nothing
        (Just _, Nothing) ->
          Just $ Update d
        (Just dcommit, Just ccommit) ->
          if T.isPrefixOf dcommit ccommit then
            Nothing -- already up to date, no change required
          else
            Just $ Update d

    diff =
      Map.mergeWithKey update (fmap Add) (fmap Remove) desired current
  in
    Map.elems diff

setupSubmodules :: Directory -> [Submodule] -> EitherT ScriptError IO ()
setupSubmodules dir desired = do
  current <- getSubmodules dir

  let
    diff =
      diffSubmodules current desired

    git =
      callFrom_ ScriptProcessError dir "git"

    add s = do
      git ["submodule", "add", renderSubmoduleUrl s, submoduleName s]
      for_ (submoduleCommit s) $ \commit ->
        callFrom_ ScriptProcessError (dir </> submoduleName s) "git" ["reset", "--hard", commit]
      git ["add", "."]

    remove s = do
      callFrom_ ScriptProcessError dir "rm" ["-rf", "./" <> submoduleName s]
      callFrom_ ScriptProcessError dir "rm" ["-rf", ".git/modules/" <> submoduleName s]

      git ["config", "-f", ".gitmodules", "--remove-section", "submodule." <> submoduleName s]
      git ["config", "-f", ".git/config", "--remove-section", "submodule." <> submoduleName s]
      git ["add", "."]

  for_ diff $ \case
    Add s ->
      add s
    Remove s ->
      remove s
    Update s -> do
      remove s
      add s

setupSandbox :: Script -> EitherT ScriptError IO ()
setupSandbox script = do
  dir <- getScriptDirectory script
  createDirectoryIfMissing True dir

  let
    git =
      dir </> ".git"

    ghci =
      dir </> ".ghci"

    cabal =
      dir </> "script.cabal"

    path =
      dir </> "script.path"

  unlessM (doesFileExist path) $
    writeUtf8 path (scriptPath script)

  unlessM (doesDirectoryExist git) $ do
    Hush <- callFrom ScriptProcessError dir "git" ["init"]
    pure ()

  setupSubmodules dir (scriptSubmodules script)

  writeChanged ghci $
    ":set -fno-warn-unrecognised-pragmas"

  writeChanged cabal $
    cabalText (scriptPackages script)


execScript :: Script -> [Argument] -> EitherT ScriptError IO ()
execScript script args = do
  dir <- getScriptDirectory script

  let
    name =
      takeFileName (scriptPath script)

    hash =
      dir </> "script.hash"

    hs =
      dir </> "script.hs"

    exe_orig =
      dir </> "dist/build/script/script"

    exe =
      dir </> "dist/build/script/" <> name

  previousHash <- readHash hash

  unless (previousHash == Just (scriptHash script)) $ do
    ignoreIO $ removeFile hash

    setupSandbox script

    writeChanged hs $
      scriptMain script

    mafia <- getExecutablePath
    Pass <- callFrom ScriptProcessError dir mafia ["build"]

    copyFile exe_orig exe

    writeUtf8 hash $
      renderHash (scriptHash script)

  exec ScriptProcessError exe args

mafiaScript :: Script -> [Argument] -> EitherT ScriptError IO ()
mafiaScript script args = do
  setupSandbox script

  dir <- getScriptDirectory script
  mafia <- getExecutablePath

  execFrom ScriptProcessError dir mafia $ args <> [scriptPath script]

runScript :: File -> [Argument] -> EitherT ScriptError IO ()
runScript file args0 = do
  path <- canonicalizePath file
  source <- hoistMaybe (ScriptFileNotFound file) =<< readUtf8 path

  let
    script =
      parseScript path source

  case args0 of
    "+MAFIA" : args ->
      mafiaScript script args
    args ->
      execScript script args
