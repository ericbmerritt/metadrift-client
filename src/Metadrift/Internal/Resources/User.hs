{-# LANGUAGE DeriveGeneric, OverloadedStrings, DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns, LambdaCase #-}

module Metadrift.Internal.Resources.User where

import qualified Data.Lens.Common as Lens
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import           GHC.Generics (Generic)
import qualified Metadrift.Internal.Service as Service
import qualified Metadrift.Internal.Service.User as Service.User
import qualified Metadrift.Internal.Resources.Support as Support
import qualified Metadrift.Internal.Utils as Utils
import qualified Network.HTTP.Simple as HTTP
import           Options.Generic (ParseRecord)
import           System.Exit (ExitCode(..))

data Command = Get T.Text
             |
               Create
                 { username :: T.Text
                 , preferredName :: T.Text
                 , email :: T.Text
                 , teams :: [T.Text]
                 }
             |
               Update
                 { uid :: T.Text
                 , op :: T.Text
                 , fieldName :: T.Text
                 , value :: T.Text
                 }
             | List
  deriving (Generic, Show)

instance ParseRecord Command

setMap :: Support.UpdateMap Service.User.T
setMap = Map.fromList
           [ ("preferredName", Lens.setL Service.User._preferredName)
           , ("email", Lens.setL Service.User._email)
           , ("teams", \val -> Lens.setL Service.User._teams [val])
           ]

addMap :: Support.UpdateMap Service.User.T
addMap = Map.fromList
           [ ("preferredName", Lens.setL Service.User._preferredName)
           , ("email", Lens.setL Service.User._email)
           , ("teams", \val obj -> let newVal = val : Lens.getL Service.User._teams obj
                                   in Lens.setL Service.User._teams newVal obj)
           ]

actionMap :: Map.Map T.Text (Support.UpdateMap Service.User.T)
actionMap = Map.fromList [("add", addMap), ("set", setMap)]

update :: T.Text -> T.Text -> T.Text -> Service.Config -> Service.User.T -> IO ExitCode
update action fieldName value config user =
  case Map.lookup action actionMap of
    Just aMap ->
      Support.setField aMap user fieldName value >>= \case
        Just newUser -> Service.patchUser config user newUser >>= Support.printBody
        Nothing      -> return $ ExitFailure 100
    Nothing -> do
      putStrLn ("Invalid action specified" ++ T.unpack action)
      return $ ExitFailure 99

doCommand :: Service.Config -> Command -> IO ExitCode
doCommand config List = do
  users <- HTTP.getResponseBody <$> Service.getUsers config
  Support.printBodies users
doCommand config Update { uid, op, fieldName, value } = do
  result <- Service.getUser config uid
  let user = HTTP.getResponseBody result
  update op fieldName value config user
doCommand config (Get gid) =
  Service.getUser config gid >>= Support.printBody
doCommand config Create { username, preferredName, email, teams } =
  Service.createUser config
    Service.User.T
      { Service.User.username
      , Service.User.preferredName
      , Service.User.email
      , Service.User.teams
      } >>= Support.printBody

main :: Service.Config -> [T.Text] -> IO ExitCode
main cfg = Utils.runCommandWithArgs "user" "Manage users in the system" $
  doCommand cfg

command :: (String, Service.Config -> [T.Text] -> IO ExitCode)
command = ("user", main)
