{-# LANGUAGE DeriveGeneric, OverloadedStrings, DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}

module Metadrift.Internal.Resources.Simulate where

import Data.Semigroup ((<>))
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified Metadrift.Internal.Resources.Support as Support
import qualified Metadrift.Internal.Service as Service
import qualified Metadrift.Internal.Service.Card as Service.Card
import qualified Metadrift.Internal.Service.Simulate
       as Service.Simulate
import qualified Metadrift.Internal.Utils as Utils
import Options.Applicative
       (Parser, (<$>), (<*>), option, auto, long, short, metavar, help,
        many, strOption, execParserPure, info, helper, fullDesc, progDesc,
        header, defaultPrefs)
import System.Exit (ExitCode(..))

data Command = Command
  { percentile :: Double
  , team :: [String]
  , tag :: [String]
  , workflow :: [String]
  } deriving (Generic, Show)

commandParser :: Parser Command
commandParser =
  Command <$>
  option
    auto
    (long "percentile" <> short 'p' <> metavar "PERCENTILE" <>
     help "percentile to return a result for") <*>
  many
    (strOption
       (long "team" <> short 'a' <> metavar "TEAM" <>
        help
          "team name to identify cards associated with the team to simulate for")) <*>
  many
    (strOption
       (long "tag" <> short 't' <> metavar "TAG" <>
        help "the tag to identify cards to simulate for")) <*>
  many
    (strOption
       (long "workflow" <> short 'w' <>
        metavar "backlog|cardreview|todo|doing|done|archive" <>
        help "the workflow to include"))

listToMaybe :: [a] -> Maybe [a]
listToMaybe [] = Nothing
listToMaybe el = Just el

doCommand :: Service.Config -> Command -> IO ExitCode
doCommand config Command {percentile, team, tag, workflow} =
  Service.simulate
    config
    Service.Simulate.T
    { Service.Simulate.percentile
    , Service.Simulate.tags = listToMaybe $ map T.pack tag
    , Service.Simulate.teams = listToMaybe $ map T.pack team
    , Service.Simulate.workflows =
        listToMaybe $ map (Service.Card.stringToWorkflow . T.pack) workflow
    } >>=
  Support.printBody

main :: Service.Config -> [T.Text] -> IO ExitCode
main cfg args = do
  cmd <-
    Utils.handleParseResult
      "metad simulate"
      (execParserPure defaultPrefs opts (map T.unpack args))
  doCommand cfg cmd
  where
    opts =
      info
        (helper <*> commandParser)
        (fullDesc <> progDesc "Run a simulation for the specified card group" <>
         header "metad simulate - completion type simulator")

command :: (String, Service.Config -> [T.Text] -> IO ExitCode)
command = ("simulate", main)
