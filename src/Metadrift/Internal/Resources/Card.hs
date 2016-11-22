{-# LANGUAGE DeriveGeneric, OverloadedStrings, DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns, LambdaCase #-}

module Metadrift.Internal.Resources.Card where

import qualified Data.Lens.Common as Lens
import           Control.Exception (Exception, throw)
import qualified Data.Map.Strict as Map
import           Data.Maybe.Utils (forceMaybe)
import qualified Data.Text as T
import           Data.Typeable (Typeable)
import           GHC.Generics (Generic)
import qualified Metadrift.Internal.Resources.Support as Support
import qualified Metadrift.Internal.Service as Service
import qualified Metadrift.Internal.Utils as Utils
import qualified Network.HTTP.Simple as HTTP
import           Options.Generic (ParseRecord)
import           Prelude hiding (min, max)
import           System.Exit (ExitCode(..))

data Command = Get { gname :: T.Text }
             |
               Create
                 { title :: T.Text
                 , body :: T.Text
                 , workflow :: T.Text
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
                 , min :: Int
                 , max :: Int
                 }
             | List
             | Delete { dname :: T.Text }
  deriving (Generic, Show)

instance ParseRecord Command

data CommandException = InvalidWorkflowException T.Text
  deriving (Show, Typeable)

instance Exception CommandException

stringToWorkflow :: T.Text -> Service.Workflow
stringToWorkflow "backlog" = Service.Backlog
stringToWorkflow "codereview" = Service.CardReview
stringToWorkflow "todo" = Service.ToDo
stringToWorkflow "doing" = Service.Doing
stringToWorkflow "done" = Service.Done
stringToWorkflow "archive" = Service.Archive
stringToWorkflow wf = throw $ InvalidWorkflowException wf

setMap :: Support.UpdateMap Service.Card
setMap = Map.fromList
           [ ("title", Lens.setL Service._title)
           , ("body", Lens.setL Service._body)
           , ("workflow", \val obj ->
                            Lens.setL Service._workflow (stringToWorkflow val)
                              obj)
           , ("tags", \val -> Lens.setL Service._tags [val])
           ]

addMap :: Support.UpdateMap Service.Card
addMap = Map.fromList
           [ ("title", Lens.setL Service._title)
           , ("body", Lens.setL Service._body)
           , ("workflow", \val obj ->
                            Lens.setL Service._workflow (stringToWorkflow val)
                              obj)
           , ("tags", \val obj -> let newVal = val : Lens.getL Service._tags obj
                                  in Lens.setL Service._tags newVal obj)
           ]

actionMap :: Map.Map T.Text (Support.UpdateMap Service.Card)
actionMap = Map.fromList [("add", addMap), ("set", setMap)]

update :: T.Text -> T.Text -> T.Text -> Service.Config -> Service.Card -> IO ExitCode
update action fieldName value config card =
  case Map.lookup action actionMap of
    Just aMap ->
      Support.setField aMap card fieldName value >>= \case
        Just newUser -> Service.patchCard config card newUser >>= Support.printBody
        Nothing      -> return $ ExitFailure 100
    Nothing -> do
      putStrLn ("Invalid action specified" ++ T.unpack action)
      return $ ExitFailure 99

doCommand :: Service.Config -> Command -> IO ExitCode
doCommand config AddEstimate { name, uid, min, max } = do
  result <- Service.getCard config name
  let card@Service.Card { Service.estimates } = HTTP.getResponseBody result
  let newCard = card
        { Service.estimates = Service.Estimate
                                { Service.userId = uid
                                , Service.estimate = Service.Range
                                  { Service.min
                                  , Service.max
                                  }
                                } : estimates
        }
  Service.patchCard config card newCard >>= Support.printBody
doCommand config List = do
  cards <- HTTP.getResponseBody <$> Service.getCards config
  Support.printBodies (forceMaybe . Service.name) cards
doCommand config Update { name, op, fieldName, value } = do
  result <- Service.getCard config name
  let card = HTTP.getResponseBody result
  update op fieldName value config card
doCommand config Delete { dname } =
  Service.deleteCard config dname >>= Support.printBody
doCommand config Get { gname } =
  Service.getCard config gname >>= Support.printBody
doCommand config Create { title, body, workflow, tags } =
  Service.createCard config
    Service.Card
      { Service.name = Nothing
      , Service.title
      , Service.body
      , Service.workflow = stringToWorkflow workflow
      , Service.estimates = []
      , Service.tags
      } >>= Support.printBody

main :: Service.Config -> [T.Text] -> IO ExitCode
main cfg = Utils.runCommandWithArgs "card" "Manage cards in the system" $
  doCommand cfg

command :: (String, Service.Config -> [T.Text] -> IO ExitCode)
command = ("card", main)
