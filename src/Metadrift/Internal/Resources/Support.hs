{-# LANGUAGE OverloadedStrings, DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}

module Metadrift.Internal.Resources.Support where

import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Metadrift.Internal.Utils as Utils
import qualified Network.HTTP.Simple as HTTP
import System.Exit (ExitCode(..))

type UpdateMap a = Map.Map T.Text (T.Text -> a -> a)

printBody :: Aeson.ToJSON a => HTTP.Response a -> IO ExitCode
printBody result = do
  let user = HTTP.getResponseBody result
  Utils.prettyPrint user
  return ExitSuccess

printBodies :: Aeson.ToJSON a => [a] -> IO ExitCode
printBodies objs = do
  Utils.prettyPrint objs
  return ExitSuccess

setField :: UpdateMap a -> a -> T.Text -> T.Text -> IO (Maybe a)
setField updateMap obj fieldName value =
  case Map.lookup fieldName updateMap of
    Just setter -> return $ Just (setter value obj)
    Nothing -> do
      putStrLn ("Unrecognized field name: " ++ T.unpack fieldName)
      return Nothing
