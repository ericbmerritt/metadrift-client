{-# LANGUAGE OverloadedStrings, DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns, LambdaCase #-}

module Metadrift.Internal.Resources.Support where

import qualified Data.Aeson as Aeson
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Metadrift.Internal.Utils as Utils
import qualified Network.HTTP.Simple as HTTP
import           System.Exit (ExitCode(..))

type UpdateMap a = Map.Map T.Text (T.Text -> a -> a)

printBody :: Aeson.ToJSON a => HTTP.Response a -> IO ExitCode
printBody result = do
  let user = HTTP.getResponseBody result
  Utils.prettyPrint user
  return ExitSuccess

printBodies :: Aeson.ToJSON a => (a -> T.Text) -> [a] -> IO ExitCode
printBodies _ [] =
  return ExitSuccess
printBodies getter (obj:t) =
  let un = getter obj
  in do
    putStrLn (T.unpack un ++ List.replicate (80 - T.length un) '-')
    Utils.prettyPrint obj
    printBodies getter t

setField :: UpdateMap a -> a -> T.Text -> T.Text -> IO (Maybe a)
setField updateMap obj fieldName value =
  case Map.lookup fieldName updateMap of
    Just setter -> return $ Just (setter value obj)
    Nothing -> do
      putStrLn ("Unrecognized field name: " ++ T.unpack fieldName)
      return Nothing
