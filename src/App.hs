{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module App (Task (..), Timestamp, Error, _title, isTaskDone, parseTasks, serializeTasks, toggleTask) where

import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode, object, parseJSON, toJSON, withObject, (.:), (.:?), (.=))
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T
import Lens.Micro.TH (makeLensesFor)
import Protolude

type Error = T.Text

type Timestamp = Int

data Task = Task
  { title :: T.Text,
    addedAt :: Timestamp,
    finishedAt :: Maybe Timestamp
  }
  deriving (Show)

makeLensesFor
  [ ("title", "_title"),
    ("addedAt", "_addedAt"),
    ("finishedAt", "_finishedAt")
  ]
  ''Task

type TaskList = [Task]

instance FromJSON Task where
  parseJSON = withObject "Task" $ \v ->
    Task
      <$> v .: "title"
      <*> v .: "addedAt"
      <*> v .:? "finishedAt"

instance ToJSON Task where
  toJSON Task {title, addedAt, finishedAt} =
    object
      [ "title" .= title,
        "addedAt" .= addedAt,
        "finishedAt" .= finishedAt
      ]

parseTasks :: BS.ByteString -> Either Error TaskList
parseTasks bs = case eitherDecode bs of
  Left e -> Left $ T.pack e
  Right tasks -> Right tasks

serializeTasks :: TaskList -> BS.ByteString
serializeTasks = encode

isTaskDone :: Task -> Bool
isTaskDone Task {finishedAt = finishedAt} = isJust finishedAt


toggleTask :: Timestamp -> Task -> Task
toggleTask ts task@Task{finishedAt} = task
            { finishedAt = case finishedAt of
                Nothing -> Just ts
                Just _ -> Nothing
            }
