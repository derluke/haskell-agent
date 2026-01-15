{-# LANGUAGE OverloadedStrings #-}

module TypedAgentBadRun where

import Agent
import Data.Text (Text)
import Types

model :: Text
model = "azure/gpt-5-2025-08-07"

weatherAgent :: Agent () WeatherInfo
weatherAgent = typedAgent model

-- Not subtle: runAgentWithDeps expects an Agent with matching deps.
badRunWithDeps :: LLMClient c => c -> WeatherInfo -> IO (Either AgentError (WeatherInfo, AgentState))
badRunWithDeps client deps =
  runAgentWithDeps deps client weatherAgent "Weather in Tokyo?"
