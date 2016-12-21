{-# LANGUAGE DeriveGeneric, OverloadedStrings, DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns, LambdaCase #-}

module Metadrift.Internal.Resources.Card where

import           Control.Monad (mapM)
import qualified Data.Either.Utils as EitherUtils
import qualified Data.Lens.Common as Lens
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Read as Read
import qualified Data.Yaml as Yaml
import           GHC.Generics (Generic)
import qualified Metadrift.Internal.Resources.Support as Support
import qualified Metadrift.Internal.Service as Service
import qualified Metadrift.Internal.Service.Card as Service.Card
import qualified Metadrift.Internal.Utils as Utils
import qualified Network.HTTP.Simple as HTTP
import           Options.Generic (ParseRecord)
import           Prelude hiding (min, max)
import           System.Exit (ExitCode(..))
import           System.IO (FilePath)

data Command = Get { gname :: T.Text }
             | Load FilePath
             | Create
                 { title :: T.Text
                 , body :: T.Text
                 , workflow :: T.Text
                 , priority :: Double
                 , tags :: [T.Text]
                 }
             |
               Update
                 { name :: T.Text
                 , op :: T.Text
                 , fieldName :: T.Text
                 , value :: T.Text
                 }
             |
               AddEstimate
                 { name :: T.Text
                 , uid :: T.Text
                 , p5 :: Double
                 , p95 :: Double
                 }
             | List
             | Delete { dname :: T.Text }
  deriving (Generic, Show)

instance ParseRecord Command

setMap :: Support.UpdateMap Service.Card.T
setMap = Map.fromList
           [ ("title", Lens.setL Service.Card._title)
           , ("doer", Lens.setL Service.Card._doer . Just)
           , ("body", Lens.setL Service.Card._body)
           , ("priority", Lens.setL Service.Card._priority .
                          fst .
                          EitherUtils.forceEither .
                          Read.rational)
           , ("workflow", \val obj ->
                            Lens.setL
                              Service.Card._workflow
                              (Service.Card.stringToWorkflow val)
                              obj)
           , ("tags", \val -> Lens.setL Service.Card._tags [val])
           ]

addMap :: Support.UpdateMap Service.Card.T
addMap = Map.fromList
           [ ("title", Lens.setL Service.Card._title)
           , ("doer", Lens.setL Service.Card._doer . Just)
           , ("body", Lens.setL Service.Card._body)
           , ("priority", Lens.setL Service.Card._priority .
                          fst .
                          EitherUtils.forceEither .
                          Read.rational)
           , ("workflow", \val obj ->
                            Lens.setL
                              Service.Card._workflow
                              (Service.Card.stringToWorkflow val)
                              obj)
           , ("tags", \val obj -> let newVal = val : Lens.getL Service.Card._tags obj
                                  in Lens.setL Service.Card._tags newVal obj)
           ]

actionMap :: Map.Map T.Text (Support.UpdateMap Service.Card.T)
actionMap = Map.fromList [("add", addMap), ("set", setMap)]

update :: T.Text -> T.Text -> T.Text -> Service.Config -> Service.Card.T -> IO ExitCode
update action fieldName value config card =
  case Map.lookup action actionMap of
    Just aMap ->
      Support.setField aMap card fieldName value >>= \case
        Just newCard -> Service.patchCard config card newCard >>= Support.printBody
        Nothing      -> return $ ExitFailure 100
    Nothing -> do
      putStrLn ("Invalid action specified" ++ T.unpack action)
      return $ ExitFailure 99

updateCard :: Service.Config
           -> T.Text -> Service.Card.T -> IO Service.Card.T
updateCard config cardName newCard = do
  existingCard <- HTTP.getResponseBody <$> Service.getCard config cardName
  HTTP.getResponseBody <$> Service.patchCard config existingCard newCard

processCard :: Service.Config -> Service.Card.T -> IO Service.Card.T
processCard config card =
  case Service.Card.name card of
    Just cardName ->
      updateCard config cardName card
    Nothing ->
      HTTP.getResponseBody <$> Service.createCard config card

summary :: Service.Card.T -> T.Text
summary Service.Card.T { Service.Card.name = Just name, Service.Card.title } =
  T.concat [name, " - ", title]
summary Service.Card.T { Service.Card.name = Nothing, Service.Card.title } =
  T.concat ["NEW - ", title]

doCommand :: Service.Config -> Command -> IO ExitCode
doCommand config (Load filepath) = do
  results <- Yaml.decodeFile filepath :: IO (Maybe [Service.Card.T])
  case results of
    Just cards ->
      mapM (processCard config) cards >>= Support.printBodies
    Nothing -> do
      putStrLn "Failed to parse file"
      return $ ExitFailure 90

doCommand config AddEstimate { name, uid, p5, p95 } = do
  result <- Service.getCard config name
  let card@Service.Card.T { Service.Card.estimates } = HTTP.getResponseBody
                                                         result
  let newCard = card
        { Service.Card.estimates = Service.Card.Estimate
                                     { Service.Card.username = uid
                                     , Service.Card.range = Service.Card.Range
                                       { Service.Card.p5
                                       , Service.Card.p95
                                       }
                                     } : estimates
        }
  Service.patchCard config card newCard >>= Support.printBody
doCommand config List = do
  cards <- HTTP.getResponseBody <$> Service.getCards config
  Support.printBodies cards
doCommand config Update { name, op, fieldName, value } = do
  result <- Service.getCard config name
  let card = HTTP.getResponseBody result
  update op fieldName value config card
doCommand config Delete { dname } =
  Service.deleteCard config dname >>= Support.printBody
doCommand config Get { gname } =
  Service.getCard config gname >>= Support.printBody
doCommand config Create { title, body, workflow, priority, tags } =
  Service.createCard config
    Service.Card.T
      { Service.Card.name = Nothing
      , Service.Card.title
      , Service.Card.body
      , Service.Card.workflow = Service.Card.stringToWorkflow workflow
      , Service.Card.estimates = []
      , Service.Card.priority
      , Service.Card.tags
      } >>= Support.printBody

main :: Service.Config -> [T.Text] -> IO ExitCode
main cfg = Utils.runCommandWithArgs "card" "Manage cards in the system" $
  doCommand cfg

command :: (String, Service.Config -> [T.Text] -> IO ExitCode)
command = ("card", main)
