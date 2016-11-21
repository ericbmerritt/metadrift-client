{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Main where

import qualified Data.List as List
import           Data.Text as T
import qualified Data.Yaml as Yaml
import qualified Metadrift.Resources.User as Card
import qualified Metadrift.Resources.Card as User
import qualified Metadrift.Service as Service
import           System.Environment (getArgs, getEnv)
import           System.Exit (ExitCode(..), exitWith)
import           System.FilePath ((</>))

type MainFun = Service.Config -> [T.Text] -> IO ExitCode

commands :: [(String, MainFun)]
commands = [User.command, Card.command]

commandNames :: String
commandNames = List.intercalate ", " $ List.map fst commands

routeCommand :: Service.Config -> String -> [String] -> IO ()
routeCommand config cmd rest =
  let args = List.map T.pack rest
  in case lookup cmd commands of
    Just fn -> fn config args >>= exitWith
    Nothing ->
      putStrLn $ List.concat [cmd, " is not one of ", commandNames]

loadConfig :: IO Service.Config
loadConfig = do
  home <- getEnv "HOME"
  let configFile = home </> ".metad"
  config <- Yaml.decodeFile configFile
  case config of
    Just service -> return service
    Nothing -> do
      putStrLn $ "Expected configuration file at " ++ configFile
      exitWith $ ExitFailure 100

main :: IO ()
main = do
  config <- loadConfig
  getArgs >>= \case
    cmd:rest -> routeCommand config cmd rest
    _        -> putStrLn ("You must provide subcommand of: " ++ commandNames)
