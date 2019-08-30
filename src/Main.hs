{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where
import           Protolude
import           App          ( runner, saveTodosToFile, parseTodos)
import           System.IO    ( IO )

main :: IO ()
main = do
  content <- readFile "todos.txt"
  runner (saveTodosToFile "todos.txt") (parseTodos content)
