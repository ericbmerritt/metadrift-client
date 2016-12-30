{-# LANGUAGE OverloadedStrings #-}

module Metadrift.Internal.Utils where

import           Data.Char (toLower)
import           Data.Aeson (ToJSON)
import qualified Data.Aeson.TH as Aeson
import qualified Data.ByteString.Char8 as Char8
import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Yaml as Yaml
import qualified Options.Applicative as Options
import qualified Options.Generic as OptGen
import           System.Environment (getProgName)
import           System.Exit (ExitCode(..))
import           System.IO (hPutStrLn, stderr)

handleParseResult :: T.Text -> Options.ParserResult t -> (t -> IO ExitCode) -> IO ExitCode
handleParseResult _progName (Options.Success conf) handler =
  handler conf
handleParseResult progName (Options.Failure failure) _handler = do
  let (msg, exit) = Options.renderFailure failure $ T.unpack progName
  case exit of
    ExitSuccess -> putStrLn msg
    _           -> hPutStrLn stderr msg
  return $ ExitFailure 127
handleParseResult _progName (Options.CompletionInvoked _compl) _handler = do
  putStrLn "Unexpected Result"
  return $ ExitFailure 127

runCommandWithArgs :: OptGen.ParseRecord a
                   => T.Text
                   -> T.Text
                   -> (a -> IO ExitCode)
                   -> [T.Text] -> IO ExitCode
runCommandWithArgs subcommand desc handler args =
  let prefs = Options.ParserPrefs
                { Options.prefMultiSuffix = ""
                , Options.prefDisambiguate = False
                , Options.prefShowHelpOnError = False
                , Options.prefBacktrack = True
                , Options.prefColumns = 80
                }
      header = Options.header $ T.unpack desc
      info = Options.info OptGen.parseRecord header
      args' = List.map T.unpack args
  in do
    progn <- getProgName
    let progName = T.concat [T.pack progn, " ", subcommand]
    handleParseResult progName (Options.execParserPure prefs info args') handler

defaultAesonOptions :: Aeson.Options
defaultAesonOptions =
  Aeson.defaultOptions
    { Aeson.omitNothingFields = True
    , Aeson.unwrapUnaryRecords = True
    , Aeson.constructorTagModifier = map toLower
    }

prettyPrint :: (ToJSON a) => a -> IO ()
prettyPrint obj =
  Char8.putStrLn $ Yaml.encode obj
