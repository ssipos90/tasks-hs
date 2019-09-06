{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NamedFieldPuns #-}

module App where

  import           Protolude
  import           Control.Monad                  ((>=>), fail)
  import           Data.Char                      ( digitToInt )
  import           Data.Either                    ( isLeft, fromLeft, fromRight, rights )
  import           Data.List                      ( (!!) )
  import qualified Data.Text                      as T
  import           Data.Text.IO                   ( readFile )
  import           Data.Text.Read                 ( decimal )
  import           Data.Void                      ( Void )
  import           System.IO                      (IO)
  import qualified Text.Megaparsec                as TM
  import qualified Text.Megaparsec.Char           as TMC
  
  type Parser = TM.Parsec Void Text
  type Error = Text
  type ActionFn = [Todo] -> Text -> Either Error ActionSuccess

  data ActionSuccess = ActionSuccess Text [Todo]
  data Todo = Todo { todoTask :: Text
                   , todoDone :: Bool
                   } deriving (Show)
  data Action = Action { actionCommand :: Text
                       , actionFn :: ActionFn
                       , actionExample :: Text
                       }

  err :: Bool -> Error -> Either Error ()
  err True e = Left e
  err False _ = Right ()
    
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

  parseTodo :: Text -> Either Error Todo
  parseTodo str = Right (Todo task (d == '1'))
            where d = T.head str
                  task = T.drop 2 str
                  -- p = TM.runParser TMC.binDigitChar "123"
  
  parseTodos :: Text -> [Either Error Todo]
  parseTodos = map parseTodo . T.lines
  
  create :: [Todo] -> Text -> Either Error ActionSuccess
  create todos params = Right (ActionSuccess "created task" (todos ++ [Todo { todoTask = params, todoDone = False }]))
  
  toggle :: Bool -> [Todo] -> Text -> Either Error ActionSuccess
  toggle _ [] _ = Left "Error, empty list, bro!"
  toggle as todos nStr = either
                          (Left . T.pack)
                          (\(n, _) -> ActionSuccess "Finished, ah?" <$> updateTodoAt n (markTodo as) todos)
                          (decimal nStr)

  mark = toggle True
  unmark = toggle False
  
  markTodo :: Bool -> Todo -> Either Error Todo
  markTodo as (Todo task done)
    | done == as = Left "Noop!"
    | otherwise = Right Todo { todoTask=task, todoDone=as }

  updateTodoAt :: Int -> (Todo -> Either Error Todo) -> [Todo] -> Either Error [Todo]
  updateTodoAt position fn todos = err (n < 0) "Must be a strict positive, bruh!"
                                 *> err (n >= length todos) "Out of bounds?"
                                 *> ((\todo -> take n todos ++ [todo] ++ drop (n+1) todos) <$> fn (todos !! n))
                        where n = position - 1

  showAction :: Action -> Text
  showAction Action { actionCommand, actionExample } =
    T.concat [actionCommand, " - eg: ", actionExample]
  
  parseAction :: [Action] -> Text -> (Text, Maybe Action)
  parseAction _ "" = ("", Nothing)
  parseAction actions str = (T.concat args, find (\Action {actionCommand} -> actionCommand == command) actions)
    where (command : args) = T.words str 
  
  runAction :: Maybe Action -> [Todo] -> Text -> IO [Todo]
  runAction Nothing todos _ = runActionHelper "Invalid action" todos
  runAction (Just Action{actionFn}) todos params =
    either
      (`runActionHelper` todos)
      (\(ActionSuccess msg todos) -> runActionHelper msg todos)
      $ actionFn todos params
  
  runActionHelper :: Text -> [Todo] -> IO [Todo]
  runActionHelper s todos = putStrLn s >> return todos
  
  runner :: ([Todo] -> IO ()) -> [Either Error Todo] -> IO ()
  runner saveTodos eitherTodos = do
    -- TODO make sure to show the errors at some point :)
    let todos = rights eitherTodos
    putStrLn $ T.concat ["\n", showTodos todos]
    putStrLn $ T.unlines $ map showAction actions
    command <- getLine
    let (params, action) = parseAction actions command
    newTodos <- runAction action todos params
    saveTodos newTodos
    runner saveTodos $ map Right newTodos
    where
      actions =
        [ Action "create"   create   "create Fuuuu your mum...."
        , Action "mark"     mark     "mark x"
        , Action "unmark"   unmark   "unmark x"
        ]
  
  saveTodosToFile :: FilePath -> [Todo] -> IO ()
  saveTodosToFile path = writeFile path . serializeTodos
