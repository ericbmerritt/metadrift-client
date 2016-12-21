{-# LANGUAGE DeriveGeneric, OverloadedStrings, DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns, TemplateHaskell, RankNTypes #-}

module Metadrift.Internal.Service.Secret where

import qualified Data.Aeson.TH as Aeson
import           Data.Lens.Template (nameMakeLens)
import qualified Data.Text as T
import           GHC.Generics (Generic)
import qualified Metadrift.Internal.Utils as Utils

data T = T { accessKey :: T.Text, secretKey :: Maybe T.Text, user :: T.Text }
  deriving (Generic, Show)

$(Aeson.deriveJSON Utils.defaultAesonOptions ''T)

$(nameMakeLens ''T (\name -> Just $ "_" ++ name))
