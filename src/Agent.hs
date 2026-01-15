{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Agent
  ( -- * Re-exports
    module Agent.Types
  , module Agent.Tools

    -- * Agent creation
  , agent
  , withTool

    -- * Running agents
  , runAgent
  , runAgentWithDeps
  , runAgentVerbose

    -- * Events
  , AgentEvent(..)

    -- * LLM Client typeclass
  , LLMClient(..)

    -- * Anthropic Client
  , AnthropicClient
  , newClient

    -- * OpenAI-compatible Client
  , OpenAIClient
  , newOpenAIClient
  , newDataRobotClient
  ) where

import Agent.Types
import Agent.Tools
import Agent.Anthropic (AnthropicClient, newClient, sendRequest)
import Agent.OpenAI (OpenAIClient, newOpenAIClient, newDataRobotClient, sendOpenAIRequest)

import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE

-- | Events emitted during agent execution
data AgentEvent
  = EventToolCall Text Value      -- ^ Tool name and input
  | EventToolResult Text Text     -- ^ Tool name and result
  | EventThinking Text            -- ^ Model thinking/text before tools
  | EventModelResponse Text       -- ^ Final text response
  deriving (Show, Eq)

-- | Typeclass for LLM clients
class LLMClient client where
  sendLLMRequest
    :: client
    -> AgentConfig
    -> [Message]
    -> [ToolDef]
    -> IO (Either AgentError ModelResponse)

instance LLMClient AnthropicClient where
  sendLLMRequest = sendRequest

instance LLMClient OpenAIClient where
  sendLLMRequest = sendOpenAIRequest

-- | Create a new agent with default configuration
agent
  :: Text                              -- ^ Model name
  -> Text                              -- ^ System prompt
  -> (Value -> Either Text output)     -- ^ Output parser
  -> Agent () output
agent model systemPrompt parser = Agent
  { agentConfig = AgentConfig
      { acModel = model
      , acMaxTokens = 4096
      , acSystemPrompt = systemPrompt
      , acMaxRetries = 3
      }
  , agentTools = Map.empty
  , agentParseOutput = parser
  }

-- | Add a tool to an agent
withTool :: Agent deps output -> Tool deps -> Agent deps output
withTool ag tool = ag
  { agentTools = Map.insert (tdName $ toolDef tool) tool (agentTools ag)
  }

-- | Run an agent with a user prompt (no dependencies)
runAgent
  :: LLMClient client
  => client
  -> Agent () output
  -> Text
  -> IO (Either AgentError (output, AgentState))
runAgent client ag prompt = runAgentWithCallback client ag prompt (\_ -> pure ())

-- | Run an agent with dependencies
runAgentWithDeps
  :: LLMClient client
  => deps
  -> client
  -> Agent deps output
  -> Text
  -> IO (Either AgentError (output, AgentState))
runAgentWithDeps deps client ag userPrompt =
  runAgentWithDepsCallback deps client ag userPrompt (\_ -> pure ())

-- | Run an agent with verbose output (prints tool calls and results)
runAgentVerbose
  :: LLMClient client
  => client
  -> Agent () output
  -> Text
  -> IO (Either AgentError (output, AgentState))
runAgentVerbose client ag prompt = runAgentWithCallback client ag prompt printEvent
  where
    printEvent :: AgentEvent -> IO ()
    printEvent (EventToolCall name input) = do
      TIO.putStrLn $ "  \x1b[33m→ Tool call:\x1b[0m " <> name
      let inputStr = TE.decodeUtf8 $ LBS.toStrict $ encode input
      TIO.putStrLn $ "    \x1b[90m" <> truncateText 100 inputStr <> "\x1b[0m"
    printEvent (EventToolResult name result) = do
      TIO.putStrLn $ "  \x1b[32m← Result:\x1b[0m " <> name
      TIO.putStrLn $ "    \x1b[90m" <> truncateText 100 result <> "\x1b[0m"
    printEvent (EventThinking text) = do
      TIO.putStrLn $ "  \x1b[36m💭 Thinking:\x1b[0m " <> truncateText 80 text
    printEvent (EventModelResponse _) = pure ()  -- Don't print, caller handles final response

    truncateText :: Int -> Text -> Text
    truncateText n t
      | T.length t > n = T.take n t <> "..."
      | otherwise = t

-- | Run an agent with a callback for events
runAgentWithCallback
  :: LLMClient client
  => client
  -> Agent () output
  -> Text
  -> (AgentEvent -> IO ())
  -> IO (Either AgentError (output, AgentState))
runAgentWithCallback = runAgentWithDepsCallback ()

-- | Run an agent with dependencies and a callback for events
runAgentWithDepsCallback
  :: LLMClient client
  => deps
  -> client
  -> Agent deps output
  -> Text
  -> (AgentEvent -> IO ())
  -> IO (Either AgentError (output, AgentState))
runAgentWithDepsCallback deps client ag userPrompt callback = do
  let initialState = AgentState
        { asMessages = [Message User [TextPart userPrompt]]
        , asUsage = mempty
        }
  runLoop deps client ag initialState 0 callback

-- | Main agent loop
runLoop
  :: LLMClient client
  => deps
  -> client
  -> Agent deps output
  -> AgentState
  -> Int
  -> (AgentEvent -> IO ())
  -> IO (Either AgentError (output, AgentState))
runLoop deps client ag@Agent{..} state@AgentState{..} retryCount callback
  | retryCount > acMaxRetries agentConfig = pure $ Left MaxRetriesExceeded
  | otherwise = do
      let toolDefs = map toolDef $ Map.elems agentTools

      result <- sendLLMRequest client agentConfig asMessages toolDefs

      case result of
        Left err -> pure $ Left err

        Right (TextResponse text usage) -> do
          callback $ EventModelResponse text
          let newState = state
                { asMessages = asMessages ++
                    [Message Assistant [TextPart text]]
                , asUsage = asUsage <> usage
                }
          -- Try to parse as final output
          case agentParseOutput (String text) of
            Right output -> pure $ Right (output, newState)
            Left _ -> pure $ Left $ ParseError $
              "Could not parse output: " <> text

        Right (ToolUseResponse toolCalls usage) -> do
          -- Emit tool call events
          mapM_ (\tc -> callback $ EventToolCall (tcName tc) (tcInput tc)) toolCalls
          -- Execute all tool calls
          results <- executeToolCalls agentTools deps toolCalls
          -- Emit tool result events
          mapM_ (\(tc, tr) -> callback $ EventToolResult (tcName tc) (trContent tr))
                (zip toolCalls results)
          let toolParts = map ToolUsePart toolCalls
              resultParts = map ToolResultPart results
              newState = state
                { asMessages = asMessages ++
                    [ Message Assistant toolParts
                    , Message User resultParts
                    ]
                , asUsage = asUsage <> usage
                }
          -- Continue the loop
          runLoop deps client ag newState 0 callback

        Right (MixedResponse text toolCalls usage) -> do
          -- Emit thinking event
          callback $ EventThinking text
          -- Emit tool call events
          mapM_ (\tc -> callback $ EventToolCall (tcName tc) (tcInput tc)) toolCalls
          -- Execute tool calls and continue
          results <- executeToolCalls agentTools deps toolCalls
          -- Emit tool result events
          mapM_ (\(tc, tr) -> callback $ EventToolResult (tcName tc) (trContent tr))
                (zip toolCalls results)
          let assistantParts = TextPart text : map ToolUsePart toolCalls
              resultParts = map ToolResultPart results
              newState = state
                { asMessages = asMessages ++
                    [ Message Assistant assistantParts
                    , Message User resultParts
                    ]
                , asUsage = asUsage <> usage
                }
          runLoop deps client ag newState 0 callback
