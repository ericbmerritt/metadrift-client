{-# LANGUAGE DeriveGeneric, OverloadedStrings, DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns, TemplateHaskell, RankNTypes #-}

module Metadrift.Internal.Service.ProjectedCompletionDates where

import qualified Data.Time.Clock as Clock
import qualified Data.Aeson.TH as Aeson
import qualified Data.Text as T
import           GHC.Generics (Generic)
import qualified Metadrift.Internal.Utils as Utils

data ProjectedDates = ProjectedDates
  {
    p5 :: Double
  , p95 :: Double
  , startDate :: Clock.UTCTime
  , breakoutDay :: Clock.UTCTime
  , p5Date :: Clock.UTCTime
  , p50Date :: Clock.UTCTime
  , p95Date :: Clock.UTCTime
  } deriving (Generic)

$(Aeson.deriveJSON Utils.defaultAesonOptions ''ProjectedDates)

data T =
       T
  { name :: T.Text
  , title :: T.Text
  , doer :: Maybe T.Text
  , projectedDates :: Maybe ProjectedDates
  } deriving (Generic)

$(Aeson.deriveJSON Utils.defaultAesonOptions ''T)
