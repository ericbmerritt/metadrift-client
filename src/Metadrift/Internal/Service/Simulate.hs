{-# LANGUAGE DeriveGeneric, OverloadedStrings, DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns, TemplateHaskell, RankNTypes #-}

module Metadrift.Internal.Service.Simulate where

import qualified Data.Aeson.TH as Aeson
import Data.Lens.Template (nameMakeLens)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)
import qualified Metadrift.Internal.Service.Card as Card
import qualified Metadrift.Internal.Utils as Utils

data Excluded = Excluded
  { cardName :: T.Text
  , missingEstimate :: T.Text
  } deriving (Generic)

$(Aeson.deriveJSON Utils.defaultAesonOptions ''Excluded)

$(nameMakeLens ''Excluded (\name -> Just $ "_" ++ name))

data RetiredResult = RetiredResult
  { name :: T.Text
  , retiredDate :: UTCTime
  }

$(Aeson.deriveJSON Utils.defaultAesonOptions ''RetiredResult)

data Result = Result
  { completionDate :: UTCTime
  , reqPercentile :: Double
  , workingDays :: Double
  , excluded :: [Excluded]
  , retired :: [RetiredResult]
  } deriving (Generic)

$(Aeson.deriveJSON Utils.defaultAesonOptions ''Result)

$(nameMakeLens ''Result (\name -> Just $ "_" ++ name))

data T = T
  { percentile :: Double
  , teams :: Maybe [T.Text]
  , tags :: Maybe [T.Text]
  , workflows :: Maybe [Card.Workflow]
  , teamFilter :: Maybe T.Text
  , cardFilter :: Maybe T.Text
  } deriving (Generic)

$(Aeson.deriveJSON Utils.defaultAesonOptions ''T)

$(nameMakeLens ''T (\name -> Just $ "_" ++ name))
