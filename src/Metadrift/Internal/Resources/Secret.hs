{-# LANGUAGE DeriveGeneric, OverloadedStrings, DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}

module Metadrift.Internal.Resources.Secret where

import Data.Semigroup ((<>))
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified Metadrift.Internal.Resources.Support as Support
import qualified Metadrift.Internal.Service as Service
import qualified Metadrift.Internal.Service.Secret
       as Service.Secret
import qualified Metadrift.Internal.Utils as Utils
import Options.Applicative
       (Parser, (<$>), (<*>), defaultPrefs, execParserPure, fullDesc,
        header, help, helper, info, long, metavar, optional, progDesc,
        short, strOption)
import Options.Generic (ParseRecord)
import System.Exit (ExitCode(..))

data Command
  = Create { accessKey :: T.Text
           , secretKey :: Maybe T.Text
           , user :: T.Text }
  | Delete T.Text
  deriving (Generic, Show)

instance ParseRecord Command

commandParser :: Parser Command
commandParser =
  Create <$>
  (T.pack <$>
   strOption
     (long "access-key" <> short 'a' <> metavar "STRING" <>
      help "The access key to create for the user")) <*>
  optional
    (T.pack <$>
     strOption
       (long "secret-key" <> short 's' <> metavar "STRING" <>
        help "The secret key to set for the user")) <*>
  (T.pack <$>
   strOption
     (long "username" <> short 'u' <> metavar "USERNAME" <>
      help "The name of the user that owns this access/secret key"))

doCommand :: Service.Config -> Command -> IO ExitCode
doCommand config (Delete userId) =
  Service.deleteSecret config userId >>= Support.printBody
doCommand config Create {accessKey, secretKey, user} =
  Service.createSecret
    config
    Service.Secret.T
    {Service.Secret.accessKey, Service.Secret.secretKey, Service.Secret.user} >>=
  Support.printBody

main :: Service.Config -> [T.Text] -> IO ExitCode
main cfg args = do
  cmd <-
    Utils.handleParseResult
      "metad secret"
      (execParserPure defaultPrefs opts (map T.unpack args))
  doCommand cfg cmd
  where
    opts =
      info
        (helper <*> commandParser)
        (fullDesc <> progDesc "Add a secret for the specified user" <>
         header "metad secret - create api credentials for a user")

command :: (String, Service.Config -> [T.Text] -> IO ExitCode)
command = ("secret", main)
