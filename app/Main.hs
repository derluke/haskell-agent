{-# LANGUAGE OverloadedStrings #-}

module Main where

import Agent
import Data.Aeson (Value (..))
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import Tools (calculatorTool, weatherTool)

-- | Create our agent (using DataRobot's GPT-5)
myAgent :: Agent () Text
myAgent =
  agent
    "azure/gpt-5-2025-08-07"
    "You are a helpful assistant with access to a calculator and weather information. \
    \Be concise in your responses."
    parseText
    `withTool` calculatorTool
    `withTool` weatherTool
  where
    parseText (String t) = Right t
    parseText _ = Left "Expected text response"

main :: IO ()
main = do
  putStrLn "Haskell Agent Framework Demo"
  putStrLn "============================="
  putStrLn ""

  -- Get DataRobot client from environment
  clientResult <- newDataRobotClient
  case clientResult of
    Left err -> do
      TIO.putStrLn $ "Error: " <> err
      putStrLn "Make sure DATAROBOT_API_TOKEN and DATAROBOT_ENDPOINT are set"
    Right client -> do
      -- Example 1: Calculator
      putStrLn "Query: What is 42 * 17?"
      result1 <- runAgentVerbose client myAgent "What is 42 * 17?"
      case result1 of
        Left err -> putStrLn $ "Error: " ++ show err
        Right (response, state) -> do
          TIO.putStrLn $ "\x1b[1mResponse:\x1b[0m " <> response
          putStrLn $ "Tokens used: " ++ show (asUsage state)
      putStrLn ""

      -- Example 2: Weather
      putStrLn "Query: What's the weather in Tokyo?"
      result2 <- runAgentVerbose client myAgent "What's the weather in Tokyo?"
      case result2 of
        Left err -> putStrLn $ "Error: " ++ show err
        Right (response, state) -> do
          TIO.putStrLn $ "\x1b[1mResponse:\x1b[0m " <> response
          putStrLn $ "Tokens used: " ++ show (asUsage state)
      putStrLn ""

      -- Example 3: Complex expression
      putStrLn "Query: If Tokyo is 22°C, what is that in Fahrenheit? Use the calculator."
      result3 <-
        runAgentVerbose
          client
          myAgent
          "If Tokyo is 22°C, what is that in Fahrenheit? Use the formula F = C * 9/5 + 32. Use the calculator tool."
      case result3 of
        Left err -> putStrLn $ "Error: " ++ show err
        Right (response, state) -> do
          TIO.putStrLn $ "\x1b[1mResponse:\x1b[0m " <> response
          putStrLn $ "Tokens used: " ++ show (asUsage state)
