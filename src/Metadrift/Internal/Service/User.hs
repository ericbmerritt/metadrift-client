{-# LANGUAGE DeriveGeneric, OverloadedStrings, DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns, TemplateHaskell, RankNTypes #-}

module Metadrift.Internal.Service.User where

import qualified Data.Aeson.TH as Aeson
import           Data.Lens.Template (nameMakeLens)
import qualified Data.Text as T
import           GHC.Generics (Generic)
import qualified Metadrift.Internal.Utils as Utils

data T =
       T
         { username :: T.Text
         , preferredName :: T.Text
         , email :: T.Text
         , teams :: [T.Text]
         }
  deriving (Generic, Show)

$(Aeson.deriveJSON Utils.defaultAesonOptions ''T)

$(nameMakeLens ''T (\name -> Just $ "_" ++ name))
