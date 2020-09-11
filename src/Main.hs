{-# LANGUAGE OverloadedStrings #-}

module Main where

import App (initialState, parseTasks, runner, showTask)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T
import Protolude
import qualified System.IO as IO

defaultFile :: FilePath
defaultFile = "tasks.json"

main :: IO ()
main = do
  args <- getArgs
  let filename = fromMaybe defaultFile $ head args
  content <- BS.readFile filename
  case parseTasks content of
    Left error -> putStrLn $ "Error: " <> error
    Right tasks -> do
          IO.hSetBuffering IO.stdin IO.NoBuffering
          void $ runStateT (runner $ BS.writeFile filename) (initialState tasks)
