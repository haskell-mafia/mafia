{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

import           BuildInfo_project

import           Data.String
import qualified Data.Text as T

import           Options.Applicative

import           P

import           System.IO
import           System.Exit
import           X.Options.Applicative

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  dispatch parser >>= \sc ->
    case sc of
      Version ->
        putStrLn buildInfoVersion >> exitSuccess
      RunCommand DryRun c ->
        print (renderCommand c) >> exitSuccess
      RunCommand RealRun c ->
        run c

run :: Command -> IO ()
run c = case c of
  CommandInputs name ->
    putStrLn (T.unpack name)


-------------
-- DATA TYPES
-------------

-- | This application can be run for real
-- | or just return a description of what it is about to do
data RunType =
  DryRun | RealRun
  deriving (Eq, Show)

data SafeCommand =
  Version
  | RunCommand RunType Command
  deriving (Eq, Show)

data Command =
  CommandInputs T.Text
  deriving (Eq, Show)

renderCommand :: Command -> String
renderCommand (CommandInputs name) =
   show $
    T.concat
    [ "CommandInputs("
    , "name=", name, ")"
    ]


----------
-- PARSERS
----------

parser :: Parser SafeCommand
parser =
  Version <$ flag' () (short 'v' <> long "version" <> help "Version information")
  <|> RunCommand <$> flag RealRun DryRun (long "dry-run" <> hidden) <*> commandP'

commandP' :: Parser Command
commandP' = CommandInputs <$> name'

name' :: Parser T.Text
name'= fmap T.pack $ argument str $
     metavar "VIEW-NAME"
