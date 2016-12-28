{-# LANGUAGE DeriveGeneric, OverloadedStrings, DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns, LambdaCase #-}

module Metadrift.Internal.Resources.Simulate where

import qualified Data.Text as T
import           GHC.Generics (Generic)
import qualified Metadrift.Internal.Service as Service
import qualified Metadrift.Internal.Service.Simulate as Service.Simulate
import qualified Metadrift.Internal.Resources.Support as Support
import qualified Metadrift.Internal.Utils as Utils
import           Options.Generic (ParseRecord)
import           System.Exit (ExitCode(..))

data Command =
       Command
         { percentile :: Double
         , team :: [T.Text]
         , tag :: [T.Text]
         }
  deriving (Generic, Show)

instance ParseRecord Command

maybeToText :: [T.Text] -> Maybe [T.Text]
maybeToText [] =
  Nothing
maybeToText el =
  Just el

doCommand :: Service.Config -> Command -> IO ExitCode
doCommand config Command { percentile, team, tag } =
  Service.simulate config
    Service.Simulate.T
      { Service.Simulate.percentile
      , Service.Simulate.tags = maybeToText tag
      , Service.Simulate.teams = maybeToText team
      } >>= Support.printBody

main :: Service.Config -> [T.Text] -> IO ExitCode
main cfg = Utils.runCommandWithArgs "simulate" "run simulation on tags/teams" $
  doCommand cfg

command :: (String, Service.Config -> [T.Text] -> IO ExitCode)
command = ("simulate", main)
