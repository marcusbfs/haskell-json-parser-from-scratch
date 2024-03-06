module Main where

import JsonParser
import System.Environment (getArgs)

-- Simple cli

main :: IO ()
main = do
  args <- getArgs
  if null args
    then do
      putStrLn "Please specify a JSON file"
    else
      let jsonFile = head args
       in do
            putStrLn $ "Analyzing JSON file \"" ++ jsonFile ++ "\"\n"
            fileContent <- readFile jsonFile
            case runParser jsonP fileContent of
              Left e -> putStrLn $ "Error: " ++ show e
              Right (_, jobj) -> print jobj
            return ()
