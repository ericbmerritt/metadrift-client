{-# LANGUAGE DeriveGeneric, OverloadedStrings, DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns, TemplateHaskell, RankNTypes #-}

module Metadrift.Internal.Service where

import Data.Aeson (ToJSON, FromJSON, toJSON)
import qualified Data.Aeson.Diff as Diff
import qualified Data.Aeson.TH as Aeson
import qualified Data.ByteString as B
import qualified Data.Maybe as Maybe
import Data.Maybe.Utils (forceMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Encoding
import Debug.Trace
import GHC.Generics (Generic)
import qualified Metadrift.Internal.HmacAuthClient
       as HmacAuthClient
import qualified Metadrift.Internal.Service.Card as Card
import qualified
       Metadrift.Internal.Service.ProjectedCompletionDates
       as ProjectedCompletionDates
import qualified Metadrift.Internal.Service.Secret as Secret
import qualified Metadrift.Internal.Service.Simulate as Simulate
import qualified Metadrift.Internal.Service.User as User
import qualified Metadrift.Internal.Utils as Utils
import qualified Network.HTTP.Simple as HTTP

data Config = Config
  { accessKey :: T.Text
  , secretKey :: T.Text
  , namespace :: T.Text
  , baseUrl :: Maybe String
  } deriving (Generic, Show)

$(Aeson.deriveJSON Utils.defaultAesonOptions ''Config)

data Path
  = Col T.Text
  | Item (T.Text, T.Text)

version :: String
version = "v1"

getBaseUrl :: Config -> String
getBaseUrl Config {baseUrl = Just url} = url
getBaseUrl Config {baseUrl = Nothing} = "https://metadrift.talendant.com/"

authorizeRequest :: Config -> HTTP.Request -> IO HTTP.Request
authorizeRequest Config {accessKey, secretKey, namespace} =
  let fullKey = T.concat [namespace, "-", accessKey]
      _ = traceShow fullKey ()
  in HmacAuthClient.applyHmacAuth
       HmacAuthClient.defaultHmacAuthSettings
       (Encoding.encodeUtf8 fullKey)
       (Encoding.encodeUtf8 secretKey)

createPath :: Config -> Path -> B.ByteString
createPath Config {namespace} (Col resourceType) =
  Encoding.encodeUtf8 $ T.concat ["/", namespace, "/", resourceType]
createPath Config {namespace} (Item (resourceType, itemId)) =
  Encoding.encodeUtf8 $
  T.concat ["/", namespace, "/", resourceType, "/", itemId]

createRequest'
  :: ToJSON a
  => Config -> B.ByteString -> a -> B.ByteString -> IO HTTP.Request
createRequest' config method obj path = do
  req <- HTTP.parseRequest $ getBaseUrl config
  authorizeRequest
    config
    (HTTP.setRequestMethod
       method
       (HTTP.setRequestPath path (HTTP.setRequestBodyJSON obj req)))

createEmptyRequest :: Config -> B.ByteString -> B.ByteString -> IO HTTP.Request
createEmptyRequest config method path = do
  req <- HTTP.parseRequest $ getBaseUrl config
  authorizeRequest
    config
    (HTTP.setRequestMethod method (HTTP.setRequestPath path req))

createRequest :: Config -> B.ByteString -> B.ByteString -> IO HTTP.Request
createRequest config method path = do
  req <- HTTP.parseRequest $ getBaseUrl config
  authorizeRequest
    config
    (HTTP.setRequestMethod method (HTTP.setRequestPath path req))

create
  :: (ToJSON a, FromJSON a)
  => Config -> T.Text -> a -> IO (HTTP.Response a)
create config name obj = do
  req <- createRequest' config "POST" obj $ createPath config (Col name)
  HTTP.httpJSON req

get
  :: FromJSON a
  => Config -> T.Text -> T.Text -> IO (HTTP.Response a)
get config name objId = do
  req <- createRequest config "GET" $ createPath config (Item (name, objId))
  HTTP.httpJSON req

delete
  :: FromJSON a
  => Config -> T.Text -> T.Text -> IO (HTTP.Response a)
delete config name objId = do
  req <- createRequest config "DELETE" $ createPath config (Item (name, objId))
  HTTP.httpJSON req

convertToQueryString :: [(T.Text, T.Text)]
                     -> [(B.ByteString, Maybe B.ByteString)]
convertToQueryString =
  map (\(k, v) -> (Encoding.encodeUtf8 k, Just $ Encoding.encodeUtf8 v))

getAll
  :: FromJSON a
  => Config -> T.Text -> [(T.Text, T.Text)] -> IO (HTTP.Response [a])
getAll config name baseQS =
  let qs = convertToQueryString baseQS
  in do req <- createRequest config "GET" $ createPath config (Col name)
        HTTP.httpJSON $ HTTP.setRequestQueryString qs req

patch
  :: (ToJSON a, FromJSON a)
  => Config -> T.Text -> (a -> T.Text) -> a -> a -> IO (HTTP.Response a)
patch config name getId oldObj newObj =
  let p = Diff.diff (toJSON oldObj) (toJSON newObj)
  in do req <-
          createRequest' config "PATCH" p $
          createPath config (Item (name, getId oldObj))
        HTTP.httpJSON req

toTag :: T.Text -> [T.Text] -> Maybe (T.Text, T.Text)
toTag _name [] = Nothing
toTag name values = Just (name, T.intercalate "," values)

getUser :: Config -> T.Text -> IO (HTTP.Response User.T)
getUser config = get config "users"

getUsers :: Config -> Maybe T.Text -> IO (HTTP.Response [User.T])
getUsers config qfilter =
  case qfilter of
    Nothing -> getAll config "users" []
    Just sexprFilter -> getAll config "users" [("filter", sexprFilter)]

patchUser :: Config -> User.T -> User.T -> IO (HTTP.Response User.T)
patchUser config = patch config "users" User.username

createUser :: Config -> User.T -> IO (HTTP.Response User.T)
createUser config = create config "users"

getCard :: Config -> T.Text -> IO (HTTP.Response Card.T)
getCard config = get config "cards"

getCards
  :: Config
  -> [T.Text]
  -> [Card.Workflow]
  -> Maybe T.Text
  -> IO (HTTP.Response [Card.T])
getCards config tags workflows cardFilter =
  getAll config "cards" $
  Maybe.catMaybes
    [ fmap (\f -> ("filter", f)) cardFilter
    , toTag "tags" tags
    , toTag "workflows" $ map Card.workflowToString workflows
    ]

patchCard :: Config -> Card.T -> Card.T -> IO (HTTP.Response Card.T)
patchCard config = patch config "cards" (forceMaybe . Card.name)

createCard :: Config -> Card.T -> IO (HTTP.Response Card.T)
createCard config = create config "cards"

deleteCard :: Config -> T.Text -> IO (HTTP.Response Card.T)
deleteCard config = delete config "cards"

simulate :: Config -> Simulate.T -> IO (HTTP.Response Simulate.Result)
simulate config obj = do
  req <- createRequest' config "POST" obj $ createPath config (Col "simulate")
  HTTP.httpJSON req

createSecret :: Config -> Secret.T -> IO (HTTP.Response Secret.T)
createSecret config = create config "secrets"

deleteSecret :: Config -> T.Text -> IO (HTTP.Response Secret.T)
deleteSecret config = delete config "secrets"

projectedCompletionDates :: Config
                         -> IO (HTTP.Response [ProjectedCompletionDates.T])
projectedCompletionDates config = do
  req <-
    createEmptyRequest config "POST" $
    createPath config (Item ("cards", "projected-completion-dates"))
  HTTP.httpJSON req
