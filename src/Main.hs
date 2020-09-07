{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where
import           Protolude
import           App                            ( parseTasks, runner, initialState, showTask )
import qualified Data.Text                      as T
import           System.IO                      ( IO )

defaultFile :: FilePath
defaultFile = "tasks.txt"

main :: IO ()
main = do
   args <- getArgs
   content <- readFile $ fromMaybe defaultFile $ head args
   let (errors, tasks) = partitionEithers $ parseTasks content
   putStrLn $ case length errors of
      0 -> "no errors\n"
      _ -> "errors: " <> T.unlines errors <> "\n"
   runStateT runner (initialState tasks)
   return ()
