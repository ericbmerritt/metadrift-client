{-# LANGUAGE DeriveGeneric, OverloadedStrings, DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns, LambdaCase #-}

module Metadrift.Internal.Resources.User where

import qualified Data.Lens.Common as Lens
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import           GHC.Generics (Generic)
import qualified Metadrift.Internal.Service as Service
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

type UpdateMap = Map.Map T.Text (T.Text -> Service.User -> Service.User)

setMap :: UpdateMap
setMap = Map.fromList
           [ ("preferredName", Lens.setL Service._preferredName)
           , ("email", Lens.setL Service._email)
           , ("teams", \val -> Lens.setL Service._teams [val])
           ]

addMap :: UpdateMap
addMap = Map.fromList
           [ ("preferredName", Lens.setL Service._preferredName)
           , ("email", Lens.setL Service._email)
           , ("teams", \val obj -> let newVal = val : Lens.getL Service._teams obj
                                   in Lens.setL Service._teams newVal obj)
           ]

actionMap :: Map.Map T.Text UpdateMap
actionMap = Map.fromList [("add", addMap), ("set", setMap)]

printBody :: HTTP.Response Service.User -> IO ExitCode
printBody result = do
  let user = HTTP.getResponseBody result
  Utils.prettyPrint user
  return ExitSuccess

printBodies :: [Service.User] -> IO ExitCode
printBodies [] =
  return ExitSuccess
printBodies (user:t) =
  let un = Service.username user
  in do
    putStrLn (T.unpack un ++ List.replicate (80 - T.length un) '-')
    Utils.prettyPrint user
    printBodies t

setField :: UpdateMap -> Service.User -> T.Text -> T.Text -> IO (Maybe Service.User)
setField _ _user "username" _value = do
  putStrLn "username is not mutable"
  return Nothing
setField updateMap user fieldName value =
  case Map.lookup fieldName updateMap of
    Just setter -> return $ Just (setter value user)
    Nothing -> do
      putStrLn ("Unrecognized field name: " ++ T.unpack fieldName)
      return Nothing

update :: T.Text -> T.Text -> T.Text -> Service.Config -> Service.User -> IO ExitCode
update action fieldName value config user =
  case Map.lookup action actionMap of
    Just aMap ->
      setField aMap user fieldName value >>= \case
        Just newUser -> Service.patchUser config user newUser >>= printBody
        Nothing      -> return $ ExitFailure 100
    Nothing -> do
      putStrLn ("Invalid action specified" ++ T.unpack action)
      return $ ExitFailure 99

doCommand :: Service.Config -> Command -> IO ExitCode
doCommand config List = do
  users <- HTTP.getResponseBody <$> Service.getUsers config
  printBodies users
doCommand config Update { uid, op, fieldName, value } = do
  result <- Service.getUser config uid
  let user = HTTP.getResponseBody result
  update op fieldName value config user
doCommand config Get { gid } =
  Service.getUser config gid >>= printBody
doCommand config Create { username, preferredName, email, teams } =
  Service.createUser config
    Service.User
      { Service.username
      , Service.preferredName
      , Service.email
      , Service.teams
      } >>= printBody

main :: Service.Config -> [T.Text] -> IO ExitCode
main cfg = Utils.runCommandWithArgs "user" "Manage users in the system" $
  doCommand cfg

command :: (String, Service.Config -> [T.Text] -> IO ExitCode)
command = ("user", main)
