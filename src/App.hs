{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module App (Task (..), _title, isTaskDone, parseTasks) where

import Data.Aeson (FromJSON, ToJSON, eitherDecode, object, parseJSON, toJSON, withObject, (.:), (.:?), (.=))
import qualified Data.ByteString.Lazy as BS
import Data.List ((!!))
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

err :: Bool -> Error -> Either Error ()
err True e = Left e
err False _ = Right ()

pad :: Int -> Text -> Text
pad n = (<>) (T.pack $ replicate n ' ')

listIsEmpty = Left "Error, empty list, bro!"

isTaskDone :: Task -> Bool
isTaskDone Task {finishedAt = finishedAt} = isJust finishedAt

updateTaskAt :: (Task -> Either Error Task) -> Int -> TaskList -> Either Error TaskList
updateTaskAt fn position tasks
  | n < 0 = Left ("Must be a strict positive." :: Text)
  | n >= length tasks = Left ("Out of bounds." :: Text)
  | otherwise = (\task -> take n tasks ++ [task] ++ drop (n + 1) tasks) <$> fn (tasks !! n)
  where
    n = position - 1

markToggleTaskAt :: Timestamp -> Int -> TaskList -> Either Error TaskList
markToggleTaskAt timestamp =
  updateTaskAt
    ( \task ->
        Right $
          task
            { finishedAt = case finishedAt task of
                Nothing -> Just timestamp
                Just _ -> Nothing
            }
    )
