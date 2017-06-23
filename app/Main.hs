{-# LANGUAGE OverloadedStrings, LambdaCase #-}

module Main where

import qualified Data.ByteString.Char8 as B8
import qualified Data.List as List
import Data.Text as T
import qualified Data.Yaml as Yaml
import qualified Metadrift.Resources.Card as User
import qualified Metadrift.Resources.Secret as Secret
import qualified Metadrift.Resources.Simulate as Simulate
import qualified Metadrift.Resources.User as Card
import qualified Metadrift.Service as Service
import System.Environment (getArgs, getEnv)
import System.Exit (ExitCode(..), exitWith)
import System.FilePath ((</>))

type MainFun = Service.Config -> [T.Text] -> IO ExitCode

commands :: [(String, MainFun)]
commands = [User.command, Card.command, Simulate.command, Secret.command]

commandNames :: String
commandNames = List.intercalate ", " $ List.map fst commands

routeCommand :: Service.Config -> String -> [String] -> IO ()
routeCommand config cmd rest =
  let args = List.map T.pack rest
  in case lookup cmd commands of
       Just fn -> fn config args >>= exitWith
       Nothing -> putStrLn $ List.concat [cmd, " is not one of ", commandNames]

loadConfig :: IO Service.Config
loadConfig = do
  home <- getEnv "HOME"
  let configFile = home </> ".metad"
  configBody <- B8.readFile configFile
  case Yaml.decodeEither configBody of
    Right service -> return service
    Left errStr -> do
      putStrLn $ "Got error for " ++ configFile
      putStrLn errStr
      exitWith $ ExitFailure 100

main :: IO ()
main = do
  config <- loadConfig
  getArgs >>= \case
    cmd:rest -> routeCommand config cmd rest
    _ -> putStrLn ("You must provide subcommand of: " ++ commandNames)
