{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NamedFieldPuns #-}

module App where

import           Protolude
import           Control.Monad                  ((>=>), fail)
import           Data.Char                      ( digitToInt )
import           Data.Either                    ( isLeft, fromLeft, fromRight, rights, partitionEithers )
import           Data.List                      ( (!!) )
import qualified Data.Text                      as T
import qualified Data.Text.IO                   as TIO
import           Data.Text.Read                 ( decimal )
import           Data.Void                      ( Void )
import           System.IO                      (IO)
import qualified Text.Megaparsec                as TM
import qualified Text.Megaparsec.Char           as TMC
import qualified Text.Megaparsec.Char.Lexer     as L
import           Data.Time.Clock.POSIX          ( getPOSIXTime )

type Parser = TM.Parsec Void Text
type Error = Text
type Timestamp = Int
type ActionFn = [Task] -> Timestamp -> Text -> Either Error ActionSuccess

data ActionSuccess = ActionSuccess Text [Task]
data Task = Task { taskTask :: Text
                  , taskAddedAt :: Timestamp
                  , taskFinishedAt :: Maybe Timestamp
                  } deriving (Show)

data Action = Action { actionCommand :: Text
                      , actionFn :: ActionFn
                      , actionExample :: Text
                      }

taskSequence :: Parser (Timestamp, Maybe Timestamp, Text)
taskSequence = (,,)  <$> (L.decimal :: Parser Timestamp)
                      <* TMC.char ':'
                      <*> TM.optional (TM.try L.decimal :: Parser Timestamp)
                      <* TMC.char ':'
                      <*> (T.pack <$> (TM.some (TM.satisfy (const True)) :: Parser [Char]))
                      <*  TM.eof

err :: Bool -> Error -> Either Error ()
err True e = Left e
err False _ = Right ()

serializeTasks :: [Task] -> Text
serializeTasks = T.unlines . map serializeTask

serializeTask :: Task -> Text
serializeTask Task{ taskTask, taskAddedAt, taskFinishedAt } =
                show taskAddedAt <> ":" <> finishedAt <> ":" <> taskTask
            where finishedAt = case taskFinishedAt of
                                Nothing -> ""
                                Just t  -> show t

showTask :: Task -> Text
showTask Task { taskTask, taskFinishedAt } = "[" <> done <> "] " <> taskTask
                    where done = case taskFinishedAt of
                                Just _ -> "x"
                                Nothing -> " "

showTasks :: [Task] -> Text
showTasks = T.unlines . map showTask

parseTask :: Text -> Either Error Task
parseTask str = either (Left . show) (Right . helper) (TM.runParser (taskSequence <* TM.eof) "" str)
                  where helper (addedAt, finishedAt, task) = Task task addedAt finishedAt

parseTasks :: Text -> [Either Error Task]
parseTasks = map parseTask . T.lines

create :: [Task] -> Timestamp -> Text -> Either Error ActionSuccess
create tasks now params = Right (ActionSuccess "created task" (tasks ++ [Task { taskTask = params, taskAddedAt = now, taskFinishedAt = Nothing }]))

listIsEmpty = Left "Error, empty list, bro!"

mark :: [Task] -> Timestamp -> Text -> Either Error ActionSuccess
mark [] _ _ = listIsEmpty
mark tasks ts nStr = either (Left . T.pack) helper (decimal nStr)
                    where helper a = ActionSuccess "Finished, ah?" <$> updateTaskAt (fst a) (markTask ts) tasks
                          markTask ts Task{taskTask, taskAddedAt, taskFinishedAt} = case taskFinishedAt of
                            Nothing -> Right Task { taskTask, taskAddedAt, taskFinishedAt = Just ts }
                            Just oldTs -> Left ( "Already finished at " <> show oldTs <> "!" :: Text )

unmark :: [Task] -> Timestamp -> Text -> Either Error ActionSuccess
unmark [] _ _ = listIsEmpty
unmark tasks ts nStr = either (Left . T.pack) helper (decimal nStr)
                    where helper a = ActionSuccess "Whoopsie" <$> updateTaskAt (fst a) (unmarkTask ts) tasks
                          unmarkTask ts Task{taskTask, taskAddedAt, taskFinishedAt} = case taskFinishedAt of
                            Nothing -> Left "Already in progress."
                            Just _ -> Right Task { taskTask, taskAddedAt, taskFinishedAt = Nothing }

remove :: [Task] -> Timestamp -> Text -> Either Error ActionSuccess
remove [] _ _ = listIsEmpty
remove tasks ts nStr = either (Left . T.pack) (helper . fst) (decimal nStr)
                      where helper p = Right $ ActionSuccess "Removed" $ take (p - 1) tasks ++ drop p tasks

updateTaskAt :: Int -> (Task -> Either Error Task) -> [Task] -> Either Error [Task]
updateTaskAt position fn tasks = err (n < 0) "Must be a strict positive, bruh!"
                                *> err (n >= length tasks) "Out of bounds?"
                                *> ((\task -> take n tasks ++ [task] ++ drop (n+1) tasks) <$> fn (tasks !! n))
                            where n = position - 1

showAction :: Action -> Text
showAction Action { actionCommand, actionExample } = actionCommand <> " - eg: " <> actionExample

parseAction :: Text -> State TasksState (Text, Maybe Action)
parseAction "" = return ("", Nothing)
parseAction  str = do
  TasksState {actions} <- get
  let (command : args) = T.words str
  let predicate Action {actionCommand} = actionCommand == command
  return (T.unwords args, find predicate actions)

postActionHandler :: ([Task] -> IO ()) -> [Task] -> Either Error ActionSuccess -> IO [Task]
postActionHandler _ tasks (Left e)  = putStrLn e >> return tasks
postActionHandler saveTasks _ (Right (ActionSuccess msg tasks)) = putStrLn msg >> saveTasks tasks >> return tasks

data TasksState = TasksState
                { tasks:: [Task]
                , actions :: [Action]
                }

runAction :: ActionFn -> Text -> StateT TasksState IO ()
runAction actionFn params = do
  now <- round <$> liftIO Data.Time.Clock.POSIX.getPOSIXTime
  currentState <- get
  case actionFn (tasks currentState) now params of
    Left e -> liftIO $ TIO.putStrLn e
    Right (ActionSuccess message tasks) -> do
      put (currentState { tasks })
      liftIO $ TIO.putStrLn message

runner :: StateT TasksState IO ()
runner = forever $ do
  TasksState { actions, tasks } <- get
  liftIO $ TIO.putStrLn $ showTasks tasks
  liftIO $ TIO.putStrLn $ T.unlines $ map showAction actions
  commandStr <- liftIO getLine
  (params, action) <- state . runState $ parseAction commandStr
  case action of
    Nothing -> liftIO $ TIO.putStrLn "Unknown action\n"
    Just Action{actionFn} -> runAction actionFn params
  return ()

initialState :: [Task] -> TasksState
initialState tasks = TasksState { tasks
                                , actions = [ Action "create"   create   "create Fuuuu your mum...."
                                            , Action "mark"     mark     "mark x"
                                            , Action "unmark"   unmark   "unmark x"
                                            , Action "remove"   remove   "remove x"
                                            ]
                                }
