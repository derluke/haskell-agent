{-# LANGUAGE OverloadedStrings #-}

module TypedAgentBadDepsOrder where

import Agent
import Data.Text (Text)
import Types

model :: Text
model = "azure/gpt-5-2025-08-07"

reportAgent :: Agent (WeatherInfo, ConvertedTemperature) WeatherReport
reportAgent = typedAgentWithDeps model

-- Not subtle: tuple order matters. This should fail.
badDepsOrder :: Agent (ConvertedTemperature, WeatherInfo) WeatherReport
badDepsOrder = reportAgent
