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
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Tools (calculatorTool, weatherTool)
import Types

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

-- | Run the pipeline
runPipeline :: (LLMClient c) => c -> Text -> IO (Either AgentError WeatherReport)
runPipeline client city = runExceptT $ do
  -- Step 1: () -> WeatherInfo
  (weather, _) <-
    ExceptT $ runAgentVerbose client weatherAgent ("Weather in " <> city <> "?")
  liftIO $ putStrLn $ "  => " ++ show weather

  -- Step 2: WeatherInfo -> ConvertedTemperature
  (temps, _) <-
    ExceptT $
      runAgentWithDepsVerbose
        weather
        client
        temperatureAgent
        ("Convert " <> T.pack (show (wiTemperatureC weather)) <> "C (use the calculator tool)")
  liftIO $ putStrLn $ "  => " ++ show temps

  -- Step 3: (WeatherInfo, ConvertedTemperature) -> WeatherReport
  (report, _) <-
    ExceptT $
      runAgentWithDepsVerbose
        (weather, temps)
        client
        reportAgent
        ("Generate a weather report for " <> wiCity weather <> " using the following data: " <> T.pack (show temps) <> " and " <> T.pack (show weather))
  pure report

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
    Right client ->
      runPipeline client "Tokyo" >>= \case
        Left err -> putStrLn $ "Error: " ++ show err
        Right report -> do
          let temp = wrTemperature report
          putStrLn "\n=== WeatherReport ==="
          TIO.putStrLn $ "City:        " <> wrCity report
          TIO.putStrLn $ "Condition:   " <> wrCondition report
          putStrLn $
            "Temperature: "
              ++ show (ctCelsius temp)
              ++ "C / "
              ++ show (ctFahrenheit temp)
              ++ "F / "
              ++ show (ctKelvin temp)
              ++ "K"
          TIO.putStrLn $ "Feels like:  " <> wrFeelsLike report
