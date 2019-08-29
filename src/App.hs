module App where

  import           Protolude
  import           Control.Monad                  ((>=>))
  import           Data.Char                      ( digitToInt )
  import           Data.Either                    ( isLeft, fromLeft, fromRight )
  import           Data.List                      ( (!!) )
  import qualified Data.Text                      as T
  import           Data.Text.IO                   ( readFile )
  import           Data.Text.Read                 ( decimal )
  import           System.IO                      (IO)
  
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
  serializeTodos = T.unlines . map serializeTodo
  
  serializeTodo :: Todo -> Text
  serializeTodo Todo { todoTask, todoDone } = T.concat [show $ fromEnum todoDone , ":", todoTask]
  
  showTodo :: Todo -> Text
  showTodo Todo { todoTask, todoDone } = T.concat ["[", showDone todoDone, "] ", todoTask]
  
  showTodos :: [Todo] -> Text
  showTodos = T.unlines . map (\(n, todo) -> T.concat [show n, ". ", showTodo todo]) . zip [1..]
  
  showDone :: Bool -> Text
  showDone False = " "
  showDone True = "x"
  
  parseTodo :: Text -> Todo
  parseTodo str = Todo task (d == '1')
            where d = T.head str
                  task = T.drop 2 str
  
  parseTodos :: Text -> [Todo]
  parseTodos = map parseTodo . T.lines
  
  create :: [Todo] -> Text -> Either Error ActionSuccess
  create todos params = Right (ActionSuccess "created task" (todos ++ [Todo { todoTask = params, todoDone = False }]))
  
  toggle :: Bool -> [Todo] -> Text -> Either Error ActionSuccess
  toggle _ [] _ = Left "Error, empty list, bro!"
  toggle as todos nStr = case decimal nStr of
      Left e -> Left (T.pack e)
      Right(n,_) -> fmap (ActionSuccess "E gata, ah?") (updateTodoAt n (markTodo as) todos)
  
  mark = toggle True
  unmark = toggle False
  
  markTodo :: Bool -> Todo -> Either Error Todo
  markTodo as (Todo task done)
    | done == as = Left "Noop!"
    | otherwise = Right Todo { todoTask=task, todoDone=as }
    
  updateTodoAt :: Int -> (Todo -> Either Error Todo) -> [Todo] -> Either Error [Todo]
  updateTodoAt k fn todos
                  | n < 0 = Left "Must be a strict positive, bruh!"
                  | n >= length todos = Left "Out of bounds?"
                  | otherwise = case fn (todos!!n) of 
                          Left e -> Left e
                          Right todo -> Right(take n todos ++ [todo] ++ drop (n+1) todos)
                    where n = k - 1;

  showAction :: Action -> Text
  showAction Action { actionCommand, actionExample } =
    T.concat [actionCommand, " - eg: ", actionExample]
  
  parseAction :: [Action] -> Text -> (Text, Maybe Action)
  parseAction _ "" = ("", Nothing)
  parseAction actions str = (T.concat rest, find (\Action {actionCommand} -> command == actionCommand) actions)
    where (command : rest) = T.words str 
  
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
    putStrLn $ T.concat ["\n", showTodos todos]
    putStrLn $ T.unlines $ map showAction actions
    command <- getLine
    let (params, action) = parseAction actions command
    newTodos <- runAction action todos params
    saveTodos newTodos
    runner saveTodos newTodos
    where
      actions =
        [ Action "create"   create   "create Fuuuu your mum...."
        , Action "mark"     mark     "mark x"
        , Action "unmark"   unmark   "unmark x"
        ]
  
  saveTodosToFile :: FilePath -> [Todo] -> IO ()
  saveTodosToFile path = writeFile path . serializeTodos
