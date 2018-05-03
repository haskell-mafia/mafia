{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Mafia.Options.Applicative (
    module X
  , RunType (..)
  , SafeCommand (..)
  , eitherTextReader
  , pOption
  , command'
  , dispatch
  , cli
  , safeCommand
  , versionFlag
  , dryRunFlag

  , orDie
  , orDieWithCode
  ) where

import           Control.Monad.Trans.Either (EitherT, runEitherT)

import qualified Data.Attoparsec.Text as A
import           Data.String (String)
import qualified Data.Text as T

import           Distribution.Version (Version, versionNumbers)

import           Options.Applicative as X
import           Options.Applicative.Types as X

import           Mafia.P
import           Mafia.Cabal.Version

import           System.Exit (exitSuccess, ExitCode(..), exitWith)
import           System.IO (IO, putStrLn, print, hPutStrLn, stderr)

data RunType =
    DryRun
  | RealRun
  deriving (Eq, Show)

data SafeCommand a =
    VersionCommand
  | DependencyCommand
  | RunCommand RunType a
  deriving (Eq, Show)

showVersion :: Version -> String
showVersion = intercalate "." . fmap show . versionNumbers

-- | Turn a parser into a ReadM
eitherTextReader :: (e -> Text) -> (Text -> Either e a) -> ReadM a
eitherTextReader render f =
  eitherReader $
    first (T.unpack . render) . f . T.pack

-- | Turn apn attoparsec parser into a ReadM
pOption :: A.Parser a -> ReadM a
pOption p =
  eitherReader (A.parseOnly p . T.pack)

-- | A 'command' combinator that adds helper and description in a
--   slightly cleaner way.
command' :: String -> String -> Parser a -> Mod CommandFields a
command' label description parser =
  command label (info (parser <**> helper) (progDesc description))

-- | Dispatch multi-mode programs with appropriate helper to make the
--   default behaviour a bit better.
dispatch :: Parser a -> IO a
dispatch p = customExecParser (prefs showHelpOnEmpty) (info (p <**> helper) idm)

-- | Simple interface over 'dispatch' and 'safeCommand'
--
-- @ name -> version -> dependencyInfo -> parser -> action @
--
--   Example usage:
--
-- > cli "my-cli" buildInfoVersion dependencyInfo myThingParser $ \c ->
-- >  case c of
-- >      DoThingA -> ...
-- >      DoThingB -> ...
cli :: Show a => [Char] -> [Char] -> [[Char]] -> Parser a -> (a -> IO b) -> IO b
cli name v deps commandParser act = do
  dispatch (safeCommand commandParser) >>= \a ->
    case a of
      VersionCommand -> do
        putStrLn (name <> ": " <> v)
        eitherCV <- runEitherT getCabalVersion
        case eitherCV of
          Right cv ->
            putStrLn ("built with cabal version: " <> showVersion cv ) >> exitSuccess
          Left _ ->
            putStrLn "Unable to retrieve cabal version" >> exitSuccess
      DependencyCommand ->
        mapM putStrLn deps >> exitSuccess
      RunCommand DryRun c ->
        print c >> exitSuccess
      RunCommand RealRun c ->
        act c

-- | Turn a Parser for a command of type a into a safe command
--   with a dry-run mode and a version flag
safeCommand :: Parser a -> Parser (SafeCommand a)
safeCommand commandParser =
      VersionCommand <$ versionFlag
  <|> DependencyCommand <$ dependencyFlag
  <|> RunCommand <$> dryRunFlag <*> commandParser

versionFlag :: Parser ()
versionFlag =
  flag' () $
       short 'v'
    <> long "version"
    <> help "Version information"

dependencyFlag :: Parser ()
dependencyFlag =
  flag' () $
       long "dependencies"
    <> hidden

dryRunFlag :: Parser RunType
dryRunFlag =
  flag RealRun DryRun $
       long "dry-run"
    <> hidden

-- | orDieWithCode with an exit code of 1 in case of an error
--
orDie :: (e -> Text) -> EitherT e IO a -> IO a
orDie = orDieWithCode 1

-- | An idiom for failing hard on EitherT errors.
--
-- *This really dies*. There is no other way to say it.
--
-- The reason it lives with command line parser tooling, is that is
-- the only valid place to actually exit like this. Be appropriately
-- wary.
--
orDieWithCode :: Int -> (e -> Text) -> EitherT e IO a -> IO a
orDieWithCode code render e =
  runEitherT e >>=
    either (\err -> (hPutStrLn stderr . T.unpack . render) err >> exitWith (ExitFailure code)) pure
