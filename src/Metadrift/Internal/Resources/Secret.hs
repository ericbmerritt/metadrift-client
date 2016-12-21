{-# LANGUAGE DeriveGeneric, OverloadedStrings, DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns, LambdaCase #-}

module Metadrift.Internal.Resources.Secret where

import qualified Data.Text as T
import           GHC.Generics (Generic)
import qualified Metadrift.Internal.Service as Service
import qualified Metadrift.Internal.Service.Secret as Service.Secret
import qualified Metadrift.Internal.Resources.Support as Support
import qualified Metadrift.Internal.Utils as Utils
import           Options.Generic (ParseRecord)
import           System.Exit (ExitCode(..))

data Command =
               Create
                 { accessKey :: T.Text
                 , secretKey :: Maybe T.Text
                 , user :: T.Text
                 }
             | Delete T.Text
  deriving (Generic, Show)

instance ParseRecord Command

doCommand :: Service.Config -> Command -> IO ExitCode
doCommand config (Delete userId) =
  Service.deleteSecret config userId >>= Support.printBody
doCommand config Create { accessKey, secretKey, user } =
  Service.createSecret config
    Service.Secret.T
      { Service.Secret.accessKey
      , Service.Secret.secretKey
      , Service.Secret.user
      } >>= Support.printBody

main :: Service.Config -> [T.Text] -> IO ExitCode
main cfg = Utils.runCommandWithArgs "secrete" "Manage secretes in the system" $
  doCommand cfg

command :: (String, Service.Config -> [T.Text] -> IO ExitCode)
command = ("secret", main)
