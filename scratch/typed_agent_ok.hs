{-# LANGUAGE OverloadedStrings #-}

module TypedAgentOk where

import Agent
import Data.Text (Text)
import Tools (calculatorTool, weatherTool)
import Types

model :: Text
model = "azure/gpt-5-2025-08-07"

weatherAgent :: Agent () WeatherInfo
weatherAgent = typedAgent model `withTool` weatherTool

temperatureAgent :: Agent WeatherInfo ConvertedTemperature
temperatureAgent = typedAgentWithDeps model `withTool` liftTool calculatorTool

reportAgent :: Agent (WeatherInfo, ConvertedTemperature) WeatherReport
reportAgent = typedAgentWithDeps model

-- This compiles: dependency order matches the Agent's deps type.
okReportAgent :: Agent (WeatherInfo, ConvertedTemperature) WeatherReport
okReportAgent = reportAgent
