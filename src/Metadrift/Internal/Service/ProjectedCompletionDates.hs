{-# LANGUAGE DeriveGeneric, OverloadedStrings, DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns, TemplateHaskell, RankNTypes #-}

module Metadrift.Internal.Service.ProjectedCompletionDates where

import qualified Data.Time.Clock as Clock
import qualified Data.Aeson.TH as Aeson
import qualified Data.Text as T
import           GHC.Generics (Generic)
import qualified Metadrift.Internal.Utils as Utils

data T =
       T
         { name :: T.Text
         , title :: T.Text
         , doer :: Maybe T.Text
         , startDate :: Maybe Clock.UTCTime
         , projectedEndDate :: Maybe Clock.UTCTime
         }
  deriving Generic

$(Aeson.deriveJSON Utils.defaultAesonOptions ''T)
