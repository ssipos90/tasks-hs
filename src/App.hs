{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module App where

import Control.Monad (fail, (>=>))
import qualified Control.Monad as CM
import Data.Aeson (FromJSON, Options, ToJSON, defaultOptions, eitherDecode, encode, genericToJSON, object, parseJSON, toJSON, withObject, (.:), (.:?), (.=))
import qualified Data.ByteString.Lazy as BS
import Data.Char (digitToInt)
import Data.Either (fromLeft, fromRight, isLeft, partitionEithers, rights)
import Data.List ((!!))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Read (decimal)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Void (Void)
import Protolude
import System.Exit (exitSuccess)
import qualified System.IO as IO
import qualified Text.Read as TR

type Error = T.Text

type Timestamp = Int

data Task = Task
  { title :: T.Text,
    addedAt :: Timestamp,
    finishedAt :: Maybe Timestamp
  }
  deriving (Show)

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

newtype TasksState = TasksState {tasks :: TaskList}

runner :: (BS.ByteString -> IO ()) -> StateT TasksState IO ()
runner saver = forever $ do
  TasksState {tasks} <- get
  cmd <- liftIO $ do
    TIO.putStr "cmd: "
    IO.hFlush IO.stdout
    cmd <- IO.getChar
    TIO.putStrLn ""
    IO.hFlush IO.stdout
    return cmd
  void $ case cmd of
    --'l' -> liftIO $ putStrLn $ showTasks tasks
    'c' -> do
      title <- liftIO TIO.getLine
      now <- round <$> liftIO Data.Time.Clock.POSIX.getPOSIXTime
      tasksState <- get
      let task = Task {title = title, addedAt = now, finishedAt = Nothing}
      let TasksState {tasks} = tasksState
      put $ tasksState {tasks = tasks ++ [task]}
    'x' -> do
      nrStr <- liftIO $ do
        TIO.putStr "task: "
        IO.hFlush IO.stdout
        IO.getLine
      let nr = TR.read nrStr :: Int
      now <- round <$> liftIO Data.Time.Clock.POSIX.getPOSIXTime
      tasksState <- get
      let TasksState {tasks} = tasksState
      case markToggleTaskAt now nr tasks of
        Left e -> liftIO $ TIO.putStrLn e
        Right tasks -> do
          put $ tasksState {tasks = tasks}
          liftIO $ TIO.putStrLn "Done"
    'h' ->
      liftIO $
        TIO.putStrLn $
          T.unlines
            [ "c to create",
              "l to toggle mark",
              "x to toggle mark"
            ]
    'q' -> do
      chr <- liftIO $ do
        TIO.putStr "Really? "
        IO.hFlush IO.stdout
        chr <- IO.getChar
        TIO.putStrLn ""
        return chr
      CM.when (chr `elem` ['y', 'Y']) $ liftIO exitSuccess
    _ -> liftIO $ TIO.putStrLn "h for help"
  liftIO $ saver $ encode tasks
  return ()

initialState :: TaskList -> TasksState
initialState tasks = TasksState {tasks}
