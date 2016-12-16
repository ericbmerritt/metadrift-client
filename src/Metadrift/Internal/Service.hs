{-# LANGUAGE DeriveGeneric, OverloadedStrings, DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns, TemplateHaskell, RankNTypes #-}

module Metadrift.Internal.Service where

import           Data.Aeson (ToJSON, FromJSON, toJSON)
import qualified Data.Aeson.Diff as Diff
import qualified Data.Aeson.TH as Aeson
import qualified Data.ByteString as B
import           Data.Maybe.Utils (forceMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as Encoding
import           GHC.Generics (Generic)
import qualified Metadrift.Internal.Utils as Utils
import qualified Network.HTTP.Simple as HTTP
import qualified Network.HTTP.Types.Header as Header
import qualified Metadrift.Internal.Service.Card as Card
import qualified Metadrift.Internal.Service.User as User
import qualified Metadrift.Internal.Service.Simulate as Simulate
import qualified Metadrift.Internal.Service.Secret as Secret

data Config = Config { token :: T.Text, namespace :: T.Text }
  deriving (Generic, Show)

$(Aeson.deriveJSON Utils.defaultAesonOptions ''Config)

data Path = Col T.Text
          | Item (T.Text, T.Text)

version :: String
version = "v1"

-- baseUrl :: String baseUrl = "https://metadrift.talendant.com/"
baseUrl :: String
baseUrl = "http://localhost:3000/"

createPath :: Config -> Path -> B.ByteString
createPath Config { namespace } (Col resourceType) =
  Encoding.encodeUtf8 $ T.concat [namespace, "/", resourceType]
createPath Config { namespace } (Item (resourceType, itemId)) =
  Encoding.encodeUtf8 $ T.concat [namespace, "/", resourceType, "/", itemId]

createHeaders :: Config -> Header.RequestHeaders
createHeaders Config { token } =
  [(Header.hAuthorization, B.concat ["bearer ", Encoding.encodeUtf8 token])]

create :: (ToJSON a, FromJSON a)
       => Config
       -> T.Text
       -> a
       -> IO (HTTP.Response a)
create config name obj = do
  req' <- HTTP.parseRequest baseUrl
  let req = HTTP.setRequestMethod "POST"
              (HTTP.setRequestHeaders (createHeaders config)
                 (HTTP.setRequestPath (createPath config (Col name))
                    (HTTP.setRequestBodyJSON obj req')))
  HTTP.httpJSON req

get :: FromJSON a
    => Config
    -> T.Text
    -> T.Text
    -> IO (HTTP.Response a)
get config name objId = do
  req' <- HTTP.parseRequest baseUrl
  let req = HTTP.setRequestMethod "GET"
              (HTTP.setRequestHeaders (createHeaders config)
                 (HTTP.setRequestPath (createPath config (Item (name, objId)))
                    req'))
  HTTP.httpJSON req

delete :: FromJSON a
       => Config
       -> T.Text
       -> T.Text
       -> IO (HTTP.Response a)
delete config name objId = do
  req' <- HTTP.parseRequest baseUrl
  let req = HTTP.setRequestMethod "DELETE"
              (HTTP.setRequestHeaders (createHeaders config)
                 (HTTP.setRequestPath (createPath config (Item (name, objId)))
                    req'))
  HTTP.httpJSON req

getAll :: FromJSON a
       => Config
       -> T.Text
       -> IO (HTTP.Response [a])
getAll config name = do
  req' <- HTTP.parseRequest baseUrl
  let req = HTTP.setRequestMethod "GET"
              (HTTP.setRequestHeaders (createHeaders config)
                 (HTTP.setRequestPath (createPath config (Col name)) req'))
  HTTP.httpJSON req

patch :: (ToJSON a, FromJSON a)
      => Config
      -> T.Text
      -> (a -> T.Text)
      -> a
      -> a
      -> IO (HTTP.Response a)
patch config name getId oldObj newObj =
  let p = Diff.diff (toJSON oldObj) (toJSON newObj)
  in do
    req' <- HTTP.parseRequest baseUrl
    let req = HTTP.setRequestMethod "PATCH"
                (HTTP.setRequestHeaders (createHeaders config)
                   (HTTP.setRequestPath
                      (createPath config (Item (name, getId oldObj)))
                      (HTTP.setRequestBodyJSON p req')))
    HTTP.httpJSON req

getUser :: Config
        -> T.Text
        -> IO (HTTP.Response User.T)
getUser config = get config "users"

getUsers :: Config
         -> IO (HTTP.Response [User.T])
getUsers config =
  getAll config "users"

patchUser :: Config
          -> User.T
          -> User.T
          -> IO (HTTP.Response User.T)
patchUser config = patch config "users" User.username

createUser :: Config
           -> User.T
           -> IO (HTTP.Response User.T)
createUser config = create config "users"

getCard :: Config
        -> T.Text
        -> IO (HTTP.Response Card.T)
getCard config = get config "cards"

getCards :: Config
         -> IO (HTTP.Response [Card.T])
getCards config =
  getAll config "cards"

patchCard :: Config
          -> Card.T
          -> Card.T
          -> IO (HTTP.Response Card.T)
patchCard config = patch config "cards" (forceMaybe . Card.name)

createCard :: Config
           -> Card.T
           -> IO (HTTP.Response Card.T)
createCard config =
  create config "cards"

deleteCard :: Config
           -> T.Text
           -> IO (HTTP.Response Card.T)
deleteCard config = delete config "cards"

simulate :: Config
         -> Simulate.T
         -> IO (HTTP.Response Simulate.Result)
simulate config obj = do
  req' <- HTTP.parseRequest baseUrl
  let req = HTTP.setRequestMethod "POST"
              (HTTP.setRequestHeaders (createHeaders config)
                 (HTTP.setRequestPath (createPath config (Col "simulate"))
                    (HTTP.setRequestBodyJSON obj req')))
  HTTP.httpJSON req

createSecret :: Config
             -> Secret.T
             -> IO (HTTP.Response Secret.T)
createSecret config =
  create config "secrets"

deleteSecret :: Config
             -> T.Text
             -> IO (HTTP.Response Secret.T)
deleteSecret config = delete config "secrets"
