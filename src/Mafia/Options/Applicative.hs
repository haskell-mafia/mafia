{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Mafia.Options.Applicative (
    module X
  , RunType (..)
  , SafeCommand (..)
  , maybeTextReader
  , eitherTextReader
  , pOption
  , textRead
  , command'
  , dispatch
  , cli
  , daemon
  , safeCommand
  , versionFlag
  , dryRunFlag

  , orDie
  , orDieWithCode
  ) where

import qualified Data.Attoparsec.Text as A
import           Data.String (String)
import qualified Data.Text as T

import           Options.Applicative as X
import           Options.Applicative.Types as X

import           Mafia.P

import           System.IO (IO, putStrLn, print, hPutStrLn, stderr)
import           System.Environment (getArgs)
import           System.Exit (exitSuccess, ExitCode(..), exitWith)

import           Control.Monad.Trans.Either

data RunType =
    DryRun
  | RealRun
  deriving (Eq, Show)

data SafeCommand a =
    VersionCommand
  | DependencyCommand
  | RunCommand RunType a
  deriving (Eq, Show)

-- | Turn a parser into a ReadM
maybeTextReader :: (Text -> Maybe a) -> ReadM a
maybeTextReader f =
  eitherReader $ \s ->
    maybe (Left $ "Failed to parse: " <> s) pure . f . T.pack $ s

-- | Turn a parser into a ReadM
eitherTextReader :: (e -> Text) -> (Text -> Either e a) -> ReadM a
eitherTextReader render f =
  eitherReader $
    either (Left . T.unpack . render) Right . f . T.pack

-- | Turn apn attoparsec parser into a ReadM
pOption :: A.Parser a -> ReadM a
pOption p =
  eitherReader (A.parseOnly p . T.pack)

textRead :: ReadM Text
textRead = fmap T.pack str

-- | A 'command' combinator that adds helper and description in a
--   slightly cleaner way
command' :: String -> String -> Parser a -> Mod CommandFields a
command' label description parser =
  command label (info (parser <**> helper) (progDesc description))

-- | Dispatch multi-mode programs with appropriate helper to make the
--   default behaviour a bit better.
dispatch :: Parser a -> IO a
dispatch p = getArgs >>= \x -> case x of
  [] -> let -- We don't need to see the Missing error if we're getting the whole usage string.
            removeError' (h, e, c) = (h { helpError = mempty }, e, c)
            removeError (Failure (ParserFailure failure)) = Failure (ParserFailure ( removeError' <$> failure ))
            removeError a = a
        in  execParserPure (prefs showHelpOnError) (info (p <**> helper) idm) <$> getArgs
            >>= handleParseResult . removeError
  _  -> execParser (info (p <**> helper) idm)

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
      VersionCommand ->
        putStrLn (name <> ": " <> v) >> exitSuccess
      DependencyCommand ->
        mapM putStrLn deps >> exitSuccess
      RunCommand DryRun c ->
        print c >> exitSuccess
      RunCommand RealRun c ->
        act c

-- | Simple interface over 'dispatch' with default parsers for help,
-- 'VersionCommand', 'DependencyCommand' and 'RunCommand'.
--
-- @ name -> version -> dependencyInfo -> action @
--
--   Example usage:
--
-- > daemon "my-daemon" buildInfoVersion dependencyInfo $ do
-- >  loop my-loop
daemon :: [Char] -> [Char] -> [[Char]] -> IO a -> IO a
daemon name v deps act =
  cli name v deps (pure ()) $ \() ->
    act

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
