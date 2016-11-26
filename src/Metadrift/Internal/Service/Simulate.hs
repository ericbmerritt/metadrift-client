{-# LANGUAGE DeriveGeneric, OverloadedStrings, DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns, TemplateHaskell, RankNTypes #-}

module Metadrift.Internal.Service.Simulate where

import qualified Data.Aeson.TH as Aeson
import           Data.Lens.Template (nameMakeLens)
import qualified Data.Text as T
import           GHC.Generics (Generic)
import qualified Metadrift.Internal.Utils as Utils
import           Data.Time.Clock (UTCTime)

data Excluded = Excluded { name :: T.Text, missingEstimate :: T.Text }
  deriving (Generic)

$(Aeson.deriveJSON Utils.defaultAesonOptions ''Excluded)

$(nameMakeLens ''Excluded (\name -> Just $ "_" ++ name))

data Result =
       Result
         { completionDate :: UTCTime
         , reqPercentile :: Double
         , excluded :: [Excluded]
         }
  deriving Generic

$(Aeson.deriveJSON Utils.defaultAesonOptions ''Result)

$(nameMakeLens ''Result (\name -> Just $ "_" ++ name))

data T =
       T
         { percentile :: Double
         , teams :: Maybe [T.Text]
         , tags :: Maybe [T.Text]
         }
  deriving Generic

$(Aeson.deriveJSON Utils.defaultAesonOptions ''T)

$(nameMakeLens ''T (\name -> Just $ "_" ++ name))
