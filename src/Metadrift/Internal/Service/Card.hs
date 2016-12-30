{-# LANGUAGE DeriveGeneric, OverloadedStrings, DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns, TemplateHaskell, RankNTypes #-}

module Metadrift.Internal.Service.Card where

import           Data.Typeable (Typeable)
import           Control.Exception (Exception, throw)
import qualified Data.Aeson.TH as Aeson
import           Data.Lens.Template (nameMakeLens)
import qualified Data.Text as T
import           GHC.Generics (Generic)
import qualified Metadrift.Internal.Utils as Utils

data Range = Range { p5 :: Double, p95 :: Double }
  deriving Generic

$(Aeson.deriveJSON Utils.defaultAesonOptions ''Range)

data Estimate = Estimate { username :: T.Text, range :: Range }
  deriving Generic

$(Aeson.deriveJSON Utils.defaultAesonOptions ''Estimate)

data Workflow = Backlog
              | CardReview
              | ToDo
              | Doing
              | Done
              | Archive
  deriving (Generic, Show, Read)

$(Aeson.deriveJSON Utils.defaultAesonOptions ''Workflow)

data T =
       T
         { name :: Maybe T.Text
         , doer :: Maybe T.Text
         , title :: T.Text
         , body :: T.Text
         , estimates :: [Estimate]
         , workflow :: Workflow
         , tags :: [T.Text]
         }
  deriving (Generic)

$(Aeson.deriveJSON Utils.defaultAesonOptions ''T)

$(nameMakeLens ''T (\name -> Just $ "_" ++ name))

data CommandException = InvalidWorkflowException T.Text
  deriving (Show, Typeable)

instance Exception CommandException

stringToWorkflow :: T.Text -> Workflow
stringToWorkflow "backlog" = Backlog
stringToWorkflow "cardreview" = CardReview
stringToWorkflow "todo" = ToDo
stringToWorkflow "doing" = Doing
stringToWorkflow "done" = Done
stringToWorkflow "archive" = Archive
stringToWorkflow wf = throw $ InvalidWorkflowException wf

workflowToString :: Workflow -> T.Text
workflowToString Backlog = "backlog"
workflowToString CardReview = "cardreview"
workflowToString ToDo = "todo"
workflowToString Doing = "doing"
workflowToString Done = "done"
workflowToString Archive = "archive"
