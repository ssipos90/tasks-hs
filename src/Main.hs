{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO
import Data.Char(digitToInt)

data Todo = Todo String Int Bool deriving (Show)

data Action = Action String ([Todo] -> String -> [Todo]) String

parseTodo :: String -> Todo
parseTodo (p:d:_:task) = Todo task (digitToInt p) (d == '1')

parseTodos :: String -> [Todo]
parseTodos = map parseTodo . lines

create :: [Todo] -> String -> [Todo]
create = undefined

mark :: [Todo] -> String -> [Todo]
mark = undefined

unmark :: [Todo] -> String -> [Todo]
unmark = undefined

priority :: [Todo] -> String -> [Todo]
priority = undefined

showAction :: Action -> String
showAction (Action command _ example) = command ++ " - eg: " ++ example

matchesAction :: Action -> String -> Bool
matchesAction (Action c) command = c == command

findAction :: [Action] -> String -> Maybe Action
findAction [] _ = Nothing
findAction (a:xs) command = if matchesAction a command then a else findAction xs command

parseAction :: [Action] -> String -> (String, Maybe Action)
parseAction actions command = (rest, findAction actions cmd)
            where (cmd:rest) = words command

actions = [ Action "create" create "create \"Fuuuu....\""
          , Action "mark" mark "mark x"
          , Action "unmark" unmark "unmark x"
          , Action "priority" priority "priority x p"
          ]

handleFile :: Handle -> IO ()
handleFile h = do
            content <- hGetContents h
            putStrLn $ show $ parseTodos content
            putStrLn $ unlines $ map showAction actions
            

main :: IO ()
main = do
  withFile "todos.txt" ReadWriteMode handleFile

