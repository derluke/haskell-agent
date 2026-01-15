{-# LANGUAGE OverloadedStrings #-}

module TypedAgentBadTool where

import Agent
import Data.Text (Text)
import Tools (calculatorTool)
import Types

model :: Text
model = "azure/gpt-5-2025-08-07"

-- Subtle: calculatorTool has deps (), but this agent expects WeatherInfo deps.
-- Missing liftTool causes a type error.
badToolDeps :: Agent WeatherInfo ConvertedTemperature
badToolDeps = typedAgentWithDeps model `withTool` calculatorTool
