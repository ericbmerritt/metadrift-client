{-# LANGUAGE DeriveGeneric, OverloadedStrings, DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns, LambdaCase #-}

module Metadrift.Internal.Resources.User where

import qualified Data.Lens.Common as Lens
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import           GHC.Generics (Generic)
import qualified Metadrift.Internal.Service as Service
import qualified Metadrift.Internal.Resources.Support as Support
import qualified Metadrift.Internal.Utils as Utils
import qualified Network.HTTP.Simple as HTTP
import           Options.Generic (ParseRecord)
import           System.Exit (ExitCode(..))

data Command = Get { gid :: T.Text }
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

setMap :: Support.UpdateMap Service.User
setMap = Map.fromList
           [ ("preferredName", Lens.setL Service._preferredName)
           , ("email", Lens.setL Service._email)
           , ("teams", \val -> Lens.setL Service._teams [val])
           ]

addMap :: Support.UpdateMap Service.User
addMap = Map.fromList
           [ ("preferredName", Lens.setL Service._preferredName)
           , ("email", Lens.setL Service._email)
           , ("teams", \val obj -> let newVal = val : Lens.getL Service._teams obj
                                   in Lens.setL Service._teams newVal obj)
           ]

actionMap :: Map.Map T.Text (Support.UpdateMap Service.User)
actionMap = Map.fromList [("add", addMap), ("set", setMap)]

update :: T.Text -> T.Text -> T.Text -> Service.Config -> Service.User -> IO ExitCode
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
  Support.printBodies Service.username users
doCommand config Update { uid, op, fieldName, value } = do
  result <- Service.getUser config uid
  let user = HTTP.getResponseBody result
  update op fieldName value config user
doCommand config Get { gid } =
  Service.getUser config gid >>= Support.printBody
doCommand config Create { username, preferredName, email, teams } =
  Service.createUser config
    Service.User
      { Service.username
      , Service.preferredName
      , Service.email
      , Service.teams
      } >>= Support.printBody

main :: Service.Config -> [T.Text] -> IO ExitCode
main cfg = Utils.runCommandWithArgs "user" "Manage users in the system" $
  doCommand cfg

command :: (String, Service.Config -> [T.Text] -> IO ExitCode)
command = ("user", main)
