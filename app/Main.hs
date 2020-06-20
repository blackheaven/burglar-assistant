module Main where

import System.Environment(getArgs)

import Assistant
import IO

main :: IO Int
main = do
  args <- getArgs
  case args of
       [fileName] -> do
         content <- readFile fileName
         case parseMapFile content of
           Just environment -> do
             putStrLn $ printRounded $ findLowestDetectionProbability environment
             return 0
           _ -> return 1
       _ -> return 1
