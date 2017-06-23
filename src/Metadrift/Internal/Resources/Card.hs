{-# LANGUAGE DeriveGeneric, OverloadedStrings, DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns, LambdaCase #-}

module Metadrift.Internal.Resources.Card where

import Control.Monad (mapM)
import qualified Data.Lens.Common as Lens
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import Data.Semigroup ((<>))
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import GHC.Generics (Generic)
import qualified Metadrift.Internal.Resources.Support as Support
import qualified Metadrift.Internal.Service as Service
import qualified Metadrift.Internal.Service.Card as Service.Card
import qualified Metadrift.Internal.Utils as Utils
import qualified Network.HTTP.Simple as HTTP
import Options.Applicative
       (Parser, (<$>), (<*>), option, auto, long, short, metavar, help,
        many, strOption, execParserPure, info, helper, fullDesc, progDesc,
        header, argument, str, subparser, optional, defaultPrefs, flag,
        pure)
import qualified Options.Applicative as OptParse
import Prelude hiding (min, max)
import System.Exit (ExitCode(..))
import System.IO (FilePath)

data Command
  = Get T.Text
  | Load FilePath
  | Own { cardName :: T.Text
       ,  username :: T.Text}
  | Create { title :: T.Text
          ,  doer :: Maybe T.Text
          ,  body :: T.Text
          ,  workflow :: T.Text
          ,  tags :: [T.Text]}
  | Update { name :: T.Text
          ,  op :: T.Text
          ,  fieldName :: T.Text
          ,  value :: T.Text}
  | SetEstimate { name :: T.Text
               ,  uid :: T.Text
               ,  p5 :: Double
               ,  p95 :: Double}
  | List { fullCard :: Bool
        ,  tag :: [T.Text]
        ,  wflw :: [T.Text]}
  | Delete T.Text
  | ProjectedCompletionDates
  deriving (Generic, Show)

parseGet :: Parser Command
parseGet = Get <$> (T.pack <$> argument str (metavar "NAME"))

parseLoad :: Parser Command
parseLoad = Load <$> argument str (metavar "FILEPATH")

parseOwn :: Parser Command
parseOwn =
  Own <$>
  (T.pack <$>
   strOption
     (long "name" <> short 'n' <> metavar "STRING" <>
      help "The name of the card to own")) <*>
  (T.pack <$>
   strOption
     (long "username" <> short 'u' <> metavar "USERNAME" <>
      help "The name of the user to make the 'doer' if this card"))

parseCreate :: Parser Command
parseCreate =
  Create <$>
  (T.pack <$>
   strOption
     (long "title" <> short 'i' <> metavar "STRING" <>
      help "The one line title of the card")) <*>
  optional
    (T.pack <$>
     strOption
       (long "doer" <> short 'd' <> metavar "USERNAME" <>
        help "The person doing the work descriped in the card")) <*>
  (T.pack <$>
   strOption
     (long "body" <> short 'b' <> metavar "STRING" <>
      help "The body of the card")) <*>
  (T.pack <$>
   strOption
     (long "workflow" <> short 'w' <>
      metavar "backlog|cardreview|todo|doing|done|archive" <>
      help "The workflow stage this card is in")) <*>
  many
    (T.pack <$>
     strOption
       (long "tag" <> short 't' <> metavar "STRING" <>
        help "A tag associated with this card"))

parseUpdate :: Parser Command
parseUpdate =
  Update <$>
  (T.pack <$>
   strOption
     (long "name" <> short 'n' <> metavar "STRING" <>
      help "The identifying name for this card")) <*>
  (T.pack <$>
   strOption
     (long "op" <> short 'o' <> metavar "add|set" <>
      help "The operation to perform on the specified field")) <*>
  (T.pack <$>
   strOption
     (long "fieldName" <> short 'f' <> metavar "STRING" <>
      help "The name of the field to update")) <*>
  (T.pack <$>
   strOption
     (long "value" <> short 'v' <> metavar "STRING" <>
      help "The value to set or add to the field"))

parseSetEstimate :: Parser Command
parseSetEstimate =
  SetEstimate <$>
  (T.pack <$>
   strOption
     (long "name" <> short 'n' <> metavar "STRING" <>
      help "The name of the card")) <*>
  (T.pack <$>
   strOption
     (long "username" <> short 'u' <> metavar "USERNAME" <>
      help "The person to which this estimate belongs")) <*>
  option
    auto
    (long "p5" <> short '5' <> metavar "DOUBLE" <>
     help "The 5 percent level confidence estimate for this card") <*>
  option
    auto
    (long "p95" <> short '9' <> metavar "DOUBLE" <>
     help "The 95 percent level confidence estimate for this card")

parseList :: Parser Command
parseList =
  List <$>
  flag
    False
    True
    (long "long" <> short 'l' <>
     help "Print full card information for each card") <*>
  many
    (T.pack <$>
     strOption
       (long "tag" <> short 't' <> metavar "STRING" <>
        help "List only cards with this tag")) <*>
  many
    (T.pack <$>
     strOption
       (long "workflow" <> short 'w' <>
        metavar "backlog|cardreview|todo|doing|done|archive" <>
        help "The workflow stage to list cards for"))

parseDelete :: Parser Command
parseDelete = Get <$> (T.pack <$> argument str (metavar "NAME"))

parseProjectedCompletionDates :: Parser Command
parseProjectedCompletionDates = pure ProjectedCompletionDates

parseCommand :: Parser Command
parseCommand =
  subparser $
  OptParse.command
    "get"
    (parseGet `Utils.withInfo` "Print the details of a card") <>
  OptParse.command
    "load"
    (parseLoad `Utils.withInfo`
     "Load a file containing YAML descriptions of cards (creating or updating as needed") <>
  OptParse.command
    "own"
    (parseOwn `Utils.withInfo` "Set the `doer` field to the value provided") <>
  OptParse.command "create" (parseCreate `Utils.withInfo` "Create a new card") <>
  OptParse.command
    "update"
    (parseUpdate `Utils.withInfo` "Change fields in a card") <>
  OptParse.command
    "set-estimate"
    (parseSetEstimate `Utils.withInfo` "Set the estimate on an existing card") <>
  OptParse.command
    "list"
    (parseList `Utils.withInfo` "List cards in the system") <>
  OptParse.command
    "delete"
    (parseDelete `Utils.withInfo` "Delete a card on the system") <>
  OptParse.command
    "projected-completion-dates"
    (parseProjectedCompletionDates `Utils.withInfo`
     "Project completion dates for cards in `Doing`") <>
  OptParse.command
    "pcd"
    (parseProjectedCompletionDates `Utils.withInfo`
     "Project completion dates for cards in `Doing`")

setMap :: Support.UpdateMap Service.Card.T
setMap =
  Map.fromList
    [ ("title", Lens.setL Service.Card._title)
    , ("doer", Lens.setL Service.Card._doer . Just)
    , ("body", Lens.setL Service.Card._body)
    , ( "workflow"
      , \val obj ->
          Lens.setL
            Service.Card._workflow
            (Service.Card.stringToWorkflow val)
            obj)
    , ("tags", \val -> Lens.setL Service.Card._tags [val])
    ]

addMap :: Support.UpdateMap Service.Card.T
addMap =
  Map.fromList
    [ ("title", Lens.setL Service.Card._title)
    , ("doer", Lens.setL Service.Card._doer . Just)
    , ("body", Lens.setL Service.Card._body)
    , ( "workflow"
      , \val obj ->
          Lens.setL
            Service.Card._workflow
            (Service.Card.stringToWorkflow val)
            obj)
    , ( "tags"
      , \val obj ->
          let newVal = val : Lens.getL Service.Card._tags obj
          in Lens.setL Service.Card._tags newVal obj)
    ]

actionMap :: Map.Map T.Text (Support.UpdateMap Service.Card.T)
actionMap = Map.fromList [("add", addMap), ("set", setMap)]

update :: T.Text
       -> T.Text
       -> T.Text
       -> Service.Config
       -> Service.Card.T
       -> IO ExitCode
update action fieldName value config card =
  case Map.lookup action actionMap of
    Just aMap ->
      Support.setField aMap card fieldName value >>= \case
        Just newCard ->
          Service.patchCard config card newCard >>= Support.printBody
        Nothing -> return $ ExitFailure 100
    Nothing -> do
      putStrLn ("Invalid action specified" ++ T.unpack action)
      return $ ExitFailure 99

updateCard :: Service.Config -> T.Text -> Service.Card.T -> IO Service.Card.T
updateCard config cardName newCard = do
  existingCard <- HTTP.getResponseBody <$> Service.getCard config cardName
  HTTP.getResponseBody <$> Service.patchCard config existingCard newCard

processCard :: Service.Config -> Service.Card.T -> IO Service.Card.T
processCard config card =
  case Service.Card.name card of
    Just cardName -> updateCard config cardName card
    Nothing -> HTTP.getResponseBody <$> Service.createCard config card

makeSummary :: Service.Card.T -> T.Text
makeSummary Service.Card.T { Service.Card.name
                           , Service.Card.title
                           , Service.Card.doer
                           , Service.Card.tags
                           } =
  T.concat
    [ Maybe.fromMaybe "NEW" name
    , " - "
    , Maybe.fromMaybe "NOT ASSIGNED" doer
    , " - "
    , T.intercalate "," $ List.sort tags
    , " - "
    , title
    ]

replaceEstimate
  :: [Service.Card.Estimate]
  -> T.Text
  -> Service.Card.Estimate
  -> [Service.Card.Estimate]
  -> [Service.Card.Estimate]
replaceEstimate [] _uid newEstimate acc = newEstimate : acc
replaceEstimate (card:rest) uid newEstimate acc
  | Service.Card.username card == uid = rest ++ (newEstimate : acc)
  | otherwise = replaceEstimate rest uid newEstimate (card : acc)

doCommand :: Service.Config -> Command -> IO ExitCode
doCommand config ProjectedCompletionDates = do
  results <- HTTP.getResponseBody <$> Service.projectedCompletionDates config
  Support.printBodies results
doCommand config (Load filepath) = do
  results <- Yaml.decodeFile filepath :: IO (Maybe [Service.Card.T])
  case results of
    Just cards -> mapM (processCard config) cards >>= Support.printBodies
    Nothing -> do
      putStrLn "Failed to parse file"
      return $ ExitFailure 90
doCommand config SetEstimate {name, uid, p5, p95} = do
  result <- Service.getCard config name
  let card@Service.Card.T {Service.Card.estimates} = HTTP.getResponseBody result
  let newCard =
        card
        { Service.Card.estimates =
            replaceEstimate
              estimates
              uid
              Service.Card.Estimate
               { Service.Card.username = uid
               , Service.Card.range =
                   Service.Card.Range {Service.Card.p5, Service.Card.p95}
               }
              []
        }
  Service.patchCard config card newCard >>= Support.printBody
doCommand config List {fullCard = True, tag, wflw} = do
  cards <-
    HTTP.getResponseBody <$>
    Service.getCards config tag (map Service.Card.stringToWorkflow wflw)
  Support.printBodies cards
doCommand config List {fullCard = False, tag, wflw} = do
  cards <-
    HTTP.getResponseBody <$>
    Service.getCards config tag (map Service.Card.stringToWorkflow wflw)
  Support.printBodies $ map makeSummary cards
doCommand config Own {cardName, username} = do
  result <- Service.getCard config cardName
  let card = HTTP.getResponseBody result
  update "set" "doer" username config card
doCommand config Update {name, op, fieldName, value} = do
  result <- Service.getCard config name
  let card = HTTP.getResponseBody result
  update op fieldName value config card
doCommand config (Delete dname) =
  Service.deleteCard config dname >>= Support.printBody
doCommand config (Get gname) =
  Service.getCard config gname >>= Support.printBody
doCommand config Create {title, doer, body, workflow, tags} =
  Service.createCard
    config
    Service.Card.T
    { Service.Card.name = Nothing
    , Service.Card.title
    , Service.Card.doer
    , Service.Card.body
    , Service.Card.workflow = Service.Card.stringToWorkflow workflow
    , Service.Card.estimates = []
    , Service.Card.tags
    } >>=
  Support.printBody

main :: Service.Config -> [T.Text] -> IO ExitCode
main cfg args = do
  cmd <-
    Utils.handleParseResult
      "metad card"
      (execParserPure defaultPrefs opts (map T.unpack args))
  doCommand cfg cmd
  where
    opts =
      info
        (helper <*> parseCommand)
        (fullDesc <> progDesc "Manage cards on the system" <>
         header "metad card - manage cards on the system")

command :: (String, Service.Config -> [T.Text] -> IO ExitCode)
command = ("card", main)
