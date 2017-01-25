{-# LANGUAGE OverloadedStrings #-}

module Metadrift.Internal.Utils where

import System.IO (hPutStrLn, stderr)
import           Data.Char (toLower)
import           Data.Aeson (ToJSON)
import qualified Data.Aeson.TH as Aeson
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Yaml as Yaml
import qualified Data.Text as T
import           Options.Applicative (Parser, ParserInfo, ParserResult(..), (<*>),
                                      info, helper, progDesc, execCompletion,
                                      renderFailure)
import System.Exit (exitSuccess, exitWith, ExitCode(..))

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

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

handleParseResult :: T.Text -> ParserResult a -> IO a
handleParseResult _progn (Success a) = return a
handleParseResult progn (Failure failure) = do
  let (msg, exit) = renderFailure failure $ T.unpack progn
  case exit of
    ExitSuccess -> putStrLn msg
    _           -> hPutStrLn stderr msg
  exitWith exit
handleParseResult progn (CompletionInvoked compl) = do
  msg <- execCompletion compl $ T.unpack progn
  putStr msg
  exitSuccess
