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
  import           Data.Text.IO                   ( readFile )
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
  type ActionFn = [Todo] -> Timestamp -> Text -> Either Error ActionSuccess

  data ActionSuccess = ActionSuccess Text [Todo]
  data Todo = Todo { todoTask :: Text
                   , todoAddedAt :: Timestamp
                   , todoFinishedAt :: Maybe Timestamp
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

  serializeTodos :: [Todo] -> Text
  serializeTodos = T.unlines . map serializeTodo
  
  serializeTodo :: Todo -> Text
  serializeTodo Todo{ todoTask, todoAddedAt, todoFinishedAt } =
                  T.concat [ show todoAddedAt, ":", finishedAt, ":", todoTask]
              where finishedAt = case todoFinishedAt of
                                  Nothing -> ""
                                  Just t  -> show t
  
  showTodo :: Todo -> Text
  showTodo Todo { todoTask, todoFinishedAt } = T.concat ["[", done, "] ", todoTask]
                      where done = case todoFinishedAt of
                                  Just _ -> "x"
                                  Nothing -> " "
  
  showTodos :: [Todo] -> Text
  showTodos = T.unlines . zipWith (\k todo -> T.concat [show k, ". ", showTodo todo]) [1..]

  parseTodo :: Text -> Either Error Todo
  parseTodo str = either (Left . show) (Right . helper) (TM.runParser (taskSequence <* TM.eof) "" str)
                    where helper (addedAt, finishedAt, task) = Todo task addedAt finishedAt

  parseTodos :: Text -> [Either Error Todo]
  parseTodos = map parseTodo . T.lines
  
  create :: [Todo] -> Timestamp -> Text -> Either Error ActionSuccess
  create todos now params = Right (ActionSuccess "created task" (todos ++ [Todo { todoTask = params, todoAddedAt = now, todoFinishedAt = Nothing }]))
  
  mark :: [Todo] -> Timestamp -> Text -> Either Error ActionSuccess
  mark [] _ _ = Left "Error, empty list, bro!"
  mark todos ts nStr = either (Left . T.pack) helper (decimal nStr)
                      where helper a = ActionSuccess "Finished, ah?" <$> updateTodoAt (fst a) (markTodo ts) todos

  unmark :: [Todo] -> Timestamp -> Text -> Either Error ActionSuccess
  unmark [] _ _ = Left "Error, empty list, bro!"
  unmark todos ts nStr = either (Left . T.pack) helper (decimal nStr)
                      where helper a = ActionSuccess "Finished, ah?" <$> updateTodoAt (fst a) (unmarkTodo ts) todos
  
  -- markToggle :: [Todo] -> Timestamp -> Text -> Either Error ActionSuccess
  -- markToggle todos ts params = err (null todos) "Error, empty list, bro!"
  --                       *> either (Left . T.pack) (Right . fst) (decimal params)
  --                       >>= (\idx -> ActionSuccess "Finished, ah?"
  --                                 <$> updateTodoAt idx (unmarkTodo ts) todos)
                        
  markTodo :: Timestamp -> Todo -> Either Error Todo
  markTodo ts Todo{todoTask, todoAddedAt, todoFinishedAt} = case todoFinishedAt of
                          Nothing -> Right Todo { todoTask, todoAddedAt, todoFinishedAt = Just ts }
                          Just oldTs -> Left $ T.concat ["Already finished at ", show oldTs, "!"]

  unmarkTodo :: Timestamp -> Todo -> Either Error Todo
  unmarkTodo ts Todo{todoTask, todoAddedAt, todoFinishedAt} = case todoFinishedAt of
                          Just oldTs -> Right Todo { todoTask, todoAddedAt, todoFinishedAt = Nothing }
                          Nothing -> Left "Already in progress."

  updateTodoAt :: Int -> (Todo -> Either Error Todo) -> [Todo] -> Either Error [Todo]
  updateTodoAt position fn todos = err (n < 0) "Must be a strict positive, bruh!"
                                 *> err (n >= length todos) "Out of bounds?"
                                 *> ((\todo -> take n todos ++ [todo] ++ drop (n+1) todos) <$> fn (todos !! n))
                              where n = position - 1

  showAction :: Action -> Text
  showAction Action { actionCommand, actionExample } = T.concat [actionCommand, " - eg: ", actionExample]
  
  parseAction :: [Action] -> Text -> (Text, Maybe Action)
  parseAction _ "" = ("", Nothing)
  parseAction actions str = (T.unwords args, find predicate actions)
    where (command : args) = T.words str
          predicate Action {actionCommand} = actionCommand == command
  
  runAction :: ActionFn -> [Todo] -> Text -> IO (Either Error ActionSuccess)
  runAction actionFn todos params =
                              round 
                              <$> Data.Time.Clock.POSIX.getPOSIXTime 
                              >>= (\now -> return $ actionFn todos now params)
  
  runner :: ([Todo] -> IO ()) -> [Either Error Todo] -> IO ()
  runner saveTodos eitherTodos = do
    let (errors, todos) = partitionEithers eitherTodos
    putStrLn $ T.unlines errors
    innerRunner saveTodos todos

  postActionHandler :: ([Todo] -> IO ()) -> [Todo] -> Either Error ActionSuccess -> IO [Todo]
  postActionHandler _ todos (Left e)  = putStrLn e >> return todos
  postActionHandler saveTodos _ (Right (ActionSuccess msg todos)) = putStrLn msg >> saveTodos todos >> return todos

  innerRunner :: ([Todo] -> IO ()) -> [Todo] -> IO ()
  innerRunner saveTodos todos = do
    putStrLn $ T.concat ["\n", showTodos todos]
    putStrLn $ T.unlines $ map showAction actions
    command <- getLine
    let (params, action) = parseAction actions command
    case action of
      Nothing -> putStrLn ("Unkown action" :: Text) >> innerRunner saveTodos todos
      Just Action{actionFn} -> runAction actionFn todos params
                                >>= postActionHandler saveTodos todos
                                >>= innerRunner saveTodos
    where
      actions =
        [ Action "create"   create   "create Fuuuu your mum...."
        , Action "mark"     mark     "mark x"
        , Action "unmark"   unmark   "unmark x"
        ]
  
  saveTodosToFile :: FilePath -> [Todo] -> IO ()
  saveTodosToFile path = writeFile path . serializeTodos
