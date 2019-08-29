module Main where

import           Protolude
import           Control.Monad                  ((>=>))
import           Data.Either                    ( isLeft, fromLeft, fromRight )
import           Data.List                      ( (!!) )
import           Data.Char                      ( digitToInt )
import           Text.Read                      ( readEither )
import           Data.Text                      ( words, unwords, lines, unlines )
import           System.IO                      ( hGetContents )

data ActionSuccess = ActionSuccess Text [Todo]
type Error = Text
type ActionFn = [Todo] -> Text -> Either Error ActionSuccess

data Todo = Todo { todoTask :: Text
                 , todoDone :: Bool
                 } deriving (Show)

data Action = Action { actionCommand :: Text
                     , actionFn :: ActionFn
                     , actionExample :: Text
                     }

serializeTodos :: [Todo] -> Text
serializeTodos = unlines . map serializeTodo

serializeTodo :: Todo -> Text
serializeTodo Todo { todoTask, todoDone } = undefined

showTodo :: Todo -> Text
showTodo Todo { todoTask, todoDone } = "[" ++ showDone todoDone ++ "] " ++ todoTask

showTodos :: [Todo] -> Text
showTodos = unlines . map (\(n, todo) -> show n ++ ". " ++ showTodo todo) . zip [1..]

showDone :: Bool -> Text
showDone False = " "
showDone True = "x"

parseTodo :: Text -> Todo
parseTodo (d : _ : task) = Todo task (d == '1')

parseTodos :: Text -> [Todo]
parseTodos = map parseTodo . lines

create :: [Todo] -> Text -> Either Error ActionSuccess
create todos params = Right (ActionSuccess "created task" (todos ++ [Todo { todoTask = params, todoDone = False }]))

mark :: [Todo] -> Text -> Either Error ActionSuccess
mark [] _ = Left "Error, empty list, bro!"
mark todos nStr = fmap (markHelper todos) (readEither nStr)

-- fmap :: (a -> b) -> f a -> f b 

markHelper :: [Todo] -> Int -> Either Error ActionSuccess
markHelper todos n = fmap (ActionSuccess "E gata, ah?") (updateTodoAt n (markTodo True) todos)

updateTodoAt :: Int -> (Todo -> Either Error Todo) -> [Todo] -> Either Error [Todo]
updateTodoAt k fn todos
                | k < 1 = Left "Must be a strict positive, bruh!"
                | k > length todos = Left "Where are you goin, mate?"
                | otherwise = fuuu (k-1) (fn (todos!!(k-1))) todos

fuuu :: Int -> Either Error Todo -> [Todo] -> Either Error [Todo]
fuuu _ (Left e) _ = Left e
fuuu k (Right todo) todos = Right(take k todos ++ [todo] ++ drop (k+1) todos)

markTodo :: Bool -> Todo -> Either Error Todo
markTodo as (Todo task done) = if done == as then Left "Noop!" else Right Todo { todoTask=task, todoDone=as }

unmark :: [Todo] -> Text -> Either Error ActionSuccess
unmark = undefined

priority :: [Todo] -> Text -> Either Error ActionSuccess
priority = undefined

showAction :: Action -> Text
showAction Action { actionCommand, actionExample } =
  actionCommand ++ " - eg: " ++ actionExample

matchesAction :: Action -> Text -> Bool
matchesAction Action { actionCommand } command = actionCommand == command

findAction :: [Action] -> Text -> Maybe Action

findAction [] _ = Nothing
findAction (action : xs) command =
  if matchesAction action command then Just action else findAction xs command

parseAction :: [Action] -> Text -> (Text, Maybe Action)
parseAction actions command = (unwords rest, findAction actions cmd)
  where (cmd : rest) = words command

runAction :: Maybe Action -> [Todo] -> Text -> IO [Todo]
runAction Nothing todos _ = runActionHelper "Invalid action" todos
runAction (Just Action{actionFn}) todos params =
  either
    (`runActionHelper` todos)
    (\(ActionSuccess msg todos) -> runActionHelper msg todos)
    $ actionFn todos params

runActionHelper :: Text -> [Todo] -> IO [Todo]
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
      [ Action "create"   create   "create Fuuuu your mum...."
      , Action "mark"     mark     "mark x"
      , Action "unmark"   unmark   "unmark x"
      , Action "priority" priority "priority x p"
      ]

saveTodosToFile :: FilePath -> [Todo] -> IO ()
saveTodosToFile path = writeFile path . serializeTodos

main :: IO ()
main = do
  h <- openFile "todos.txt" ReadMode
  content <- hGetContents h
  runner (saveTodosToFile "todos.txt") (parseTodos content)
