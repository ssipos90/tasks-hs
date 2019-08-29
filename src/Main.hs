{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

module Main where

import           Text.Read                      ( readEither )
import           System.IO
import           Data.Char                      ( digitToInt )
import           Data.Either                    ( isLeft )
import           Control.Monad                  ((>=>))

data ActionSuccess = ActionSuccess String [Todo]
type Error = String
type ActionFn = [Todo] -> String -> Either Error ActionSuccess

data Todo = Todo { todoTask :: String
                 , todoDone :: Bool
                 } deriving (Show)

data Action = Action { actionCommand :: String
                     , actionFn :: ActionFn
                     , actionExample :: String
                     }

serializeTodos :: [Todo] -> String
serializeTodos = unlines . map serializeTodo

serializeTodo :: Todo -> String
serializeTodo Todo { todoTask, todoDone } = undefined

showTodo :: Todo -> String
showTodo Todo { todoTask, todoDone } = "[" ++ showDone todoDone ++ "] " ++ todoTask

showTodos :: [Todo] -> String
showTodos = unlines . map (\(n, todo) -> show n ++ ". " ++ showTodo todo) . zip [1..]

showDone :: Bool -> String
showDone False = " "
showDone True = "x"

parseTodo :: String -> Todo
parseTodo (d : _ : task) = Todo task (d == '1')

parseTodos :: String -> [Todo]
parseTodos = map parseTodo . lines

create :: [Todo] -> String -> Either Error ActionSuccess
create todos params = Right (ActionSuccess "created task" (todos ++ [Todo { todoTask = params, todoDone = False }]))

mark :: [Todo] -> String -> Either Error ActionSuccess
mark [] _ = Left "Error, empty list, bro!"
mark todos nStr = either Left (fmap (ActionSuccess "E gata, ah?") . markHelper todos) (readEither nStr)
                where markHelper todos n = updateTodoAt n (markTodo True) todos

updateTodoAt :: Int -> (Todo -> Either Error Todo) -> [Todo] -> Either Error [Todo]
updateTodoAt k fn todos
                | k < 1 = Left "Must be a strict positive, bruh!"
                | k > length todos = Left "Where are you goin, mate?"
                | otherwise = either Left (applyActionOnTodo todos k) (fn (todos!!(k-1)))

applyActionOnTodo :: [Todo] -> Int -> Todo -> Either Error [Todo]
applyActionOnTodo todos k todo = Right(take k todos ++ [todo] ++ drop (k+1) todos)

markTodo :: Bool -> Todo -> Either Error Todo
markTodo as (Todo task done) = if done == as then Left "Noop!" else Right (Todo task as)

unmark :: [Todo] -> String -> Either Error ActionSuccess
unmark = undefined

priority :: [Todo] -> String -> Either Error ActionSuccess
priority = undefined

showAction :: Action -> String
showAction Action { actionCommand, actionExample } =
  actionCommand ++ " - eg: " ++ actionExample

matchesAction :: Action -> String -> Bool
matchesAction Action { actionCommand } command = actionCommand == command

findAction :: [Action] -> String -> Maybe Action

findAction [] _ = Nothing
findAction (action : xs) command =
  if matchesAction action command then Just action else findAction xs command

parseAction :: [Action] -> String -> (String, Maybe Action)
parseAction actions command = (unwords rest, findAction actions cmd)
  where (cmd : rest) = words command

runAction :: Maybe Action -> [Todo] -> String -> IO [Todo]
runAction Nothing todos _ = runActionHelper "Invalid action" todos
runAction (Just Action{actionFn}) todos params =
  either
    (`runActionHelper` todos)
    (\(ActionSuccess msg todos) -> runActionHelper msg todos)
    $ actionFn todos params

runActionHelper :: String -> [Todo] -> IO [Todo]
runActionHelper s todos = putStrLn s >> return todos

runner :: ([Todo] -> IO ()) -> [Todo] -> IO ()
runner saveTodos todos = do
  putStrLn $ "\n" ++ showTodos todos
  putStrLn $ unlines $ map showAction actions
  command <- getLine
  let (rest, a) = parseAction actions command
  newTodos <- runAction a todos rest
  runner saveTodos newTodos
  where
    actions =
      [ Action "create"   create   "create \"Fuuuu....\""
      , Action "mark"     mark     "mark x"
      , Action "unmark"   unmark   "unmark x"
      , Action "priority" priority "priority x p"
      ]

saveTodosToFile :: FilePath -> [Todo] -> IO ()
saveTodosToFile path = writeFile path . serializeTodos

main :: IO ()
-- main = do
--   h <- openFile "todos.txt" ReadMode
--   content <- hGetContents h
--   hClose h
--   runner (saveTodosToFile "todos.txt") (parseTodos content)
main = do
  h <- openFile "range.txt" ReadMode
  content <- hGetContents h
  mapM_ putStrLn $ lines content
  hClose h
