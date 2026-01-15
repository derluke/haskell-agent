{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Typed Agent Chains Demo
--
-- This example demonstrates the elegance of Haskell's type system for agents.
-- The return type determines the output schema - no manual JSON instructions needed.
--
-- @
--   weatherAgent     :: Agent () WeatherInfo
--   temperatureAgent :: Agent WeatherInfo ConvertedTemperature
--   reportAgent      :: Agent (WeatherInfo, ConvertedTemperature) WeatherReport
-- @
module Main where

import Agent
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Tools (calculatorTool, weatherTool)
import Types

--------------------------------------------------------------------------------
-- Typed Agents
--
-- The magic: just declare the return type. The ToSchema instance generates
-- the JSON schema, and the agent automatically uses a 'final_result' tool
-- to guarantee type-safe output.
--------------------------------------------------------------------------------

-- | Step 1: Get weather data
-- The type signature alone tells the agent what to return!
weatherAgent :: Agent () WeatherInfo
weatherAgent = typedAgent model `withTool` weatherTool
  where
    model = "azure/gpt-5-2025-08-07"

-- | Step 2: Convert temperature (depends on WeatherInfo)
temperatureAgent :: Agent WeatherInfo ConvertedTemperature
temperatureAgent = typedAgentWithDeps model `withTool` liftTool calculatorTool
  where
    model = "azure/gpt-5-2025-08-07"

-- | Step 3: Generate complete report (depends on both previous outputs)
reportAgent :: Agent (WeatherInfo, ConvertedTemperature) WeatherReport
reportAgent = typedAgentWithDeps model
  where
    model = "azure/gpt-5-2025-08-07"

--------------------------------------------------------------------------------
-- Pipeline
--------------------------------------------------------------------------------

-- | Run the typed pipeline
runPipeline :: (LLMClient c) => c -> Text -> IO (Either AgentError WeatherReport)
runPipeline client city = do
  -- Step 1: Agent () WeatherInfo
  step1 <- runAgentVerbose client weatherAgent ("Weather in " <> city <> "?")
  case step1 of
    Left err -> pure $ Left err
    Right (weather, _) -> do
      putStrLn $ "  => " ++ show weather

      -- Step 2: Agent WeatherInfo ConvertedTemperature
      let prompt2 = "Convert " <> T.pack (show $ wiTemperatureC weather) <> "C"
      step2 <- runAgentWithDeps weather client temperatureAgent prompt2
      case step2 of
        Left err -> pure $ Left err
        Right (temps, _) -> do
          putStrLn $ "  => " ++ show temps

          -- Step 3: Agent (WeatherInfo, ConvertedTemperature) WeatherReport
          let prompt3 = "Generate a weather report for " <> wiCity weather
          step3 <- runAgentWithDeps (weather, temps) client reportAgent prompt3
          pure $ fmap fst step3

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "Haskell Agent Framework - Type-Driven Agents"
  putStrLn "============================================="
  putStrLn ""
  putStrLn "The return type IS the specification:"
  putStrLn "  weatherAgent     :: Agent () WeatherInfo"
  putStrLn "  temperatureAgent :: Agent WeatherInfo ConvertedTemperature"
  putStrLn "  reportAgent      :: Agent (WeatherInfo, ConvertedTemperature) WeatherReport"
  putStrLn ""

  newDataRobotClient >>= \case
    Left err -> TIO.putStrLn $ "Error: " <> err
    Right client -> do
      result <- runPipeline client "Tokyo"
      case result of
        Left err -> putStrLn $ "Error: " ++ show err
        Right report -> do
          putStrLn "\n=== WeatherReport ==="
          TIO.putStrLn $ "City:        " <> wrCity report
          TIO.putStrLn $ "Condition:   " <> wrCondition report
          putStrLn $
            "Temperature: "
              ++ show (ctCelsius $ wrTemperature report)
              ++ "C / "
              ++ show (ctFahrenheit $ wrTemperature report)
              ++ "F / "
              ++ show (ctKelvin $ wrTemperature report)
              ++ "K"
          TIO.putStrLn $ "Feels like:  " <> wrFeelsLike report
