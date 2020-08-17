{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where
import           Protolude
import           App                            ( parseTasks, runner, initialState, showTask )
import qualified Data.Text                      as T
import           System.IO                      ( IO )

file :: FilePath
file = "tasks.txt"

main :: IO ()
main = do
  content <- readFile file
  let (errors, tasks) = partitionEithers $ parseTasks content
  putStrLn $ case length errors of
     0 -> "no errors\n"
     _ -> "errors: " <> T.unlines errors <> "\n"
  runStateT runner (initialState tasks)
  return ()