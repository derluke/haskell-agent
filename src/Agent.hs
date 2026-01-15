{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Agent
  ( -- * Re-exports
    module Agent.Types
  , module Agent.Tools
  , module Agent.MCP

    -- * Agent creation (manual output parsing)
  , agent
  , agentWithDeps
  , withTool

    -- * Agent creation (typed output via result tool)
  , typedAgent
  , typedAgentWithDeps

    -- * Running agents
  , runAgent
  , runAgentWithDeps
  , runAgentVerbose
  , runAgentWithCallback
  , runAgentWithDepsCallback
  , continueAgent
  , continueAgentVerbose
  , continueAgentWithCallback

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

    -- * Output parsing helpers
  , parseJsonOutput
  ) where

import Agent.Types
import Agent.Tools
import Agent.MCP
import Agent.Anthropic (AnthropicClient, newClient, sendRequest)
import Agent.OpenAI (OpenAIClient, newOpenAIClient, newDataRobotClient, sendOpenAIRequest)

import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy(..))
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

-- | Create a new agent with dependencies (for chaining)
agentWithDeps
  :: Text                              -- ^ Model name
  -> Text                              -- ^ System prompt
  -> (Value -> Either Text output)     -- ^ Output parser
  -> Agent deps output
agentWithDeps model systemPrompt parser = Agent
  { agentConfig = AgentConfig
      { acModel = model
      , acMaxTokens = 4096
      , acSystemPrompt = systemPrompt
      , acMaxRetries = 3
      }
  , agentTools = Map.empty
  , agentParseOutput = parser
  }

--------------------------------------------------------------------------------
-- Typed Agents (with automatic result tool)
--------------------------------------------------------------------------------

-- | Create a typed agent that uses a result tool to guarantee output type.
-- The output type must have ToSchema and FromJSON instances.
-- No system prompt needed - instructions come from the result tool schema.
typedAgent
  :: (ToSchema output, FromJSON output)
  => Text                              -- ^ Model name
  -> Agent () output
typedAgent model = typedAgentWithDeps model

-- | Create a typed agent with dependencies
typedAgentWithDeps
  :: forall deps output. (ToSchema output, FromJSON output)
  => Text                              -- ^ Model name
  -> Agent deps output
typedAgentWithDeps model = Agent
  { agentConfig = AgentConfig
      { acModel = model
      , acMaxTokens = 4096
      , acSystemPrompt = "Use the final_result tool to return your answer."
      , acMaxRetries = 3
      }
  , agentTools = Map.singleton resultToolName (resultTool (Proxy :: Proxy output))
  , agentParseOutput = parseJsonOutput
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

-- | Continue a conversation with existing state
continueAgent
  :: LLMClient client
  => client
  -> Agent () output
  -> AgentState           -- ^ Previous conversation state
  -> Text                 -- ^ New user message
  -> IO (Either AgentError (output, AgentState))
continueAgent client ag prevState newPrompt =
  continueAgentWithCallback client ag prevState newPrompt (\_ -> pure ())

-- | Continue a conversation with verbose output
continueAgentVerbose
  :: LLMClient client
  => client
  -> Agent () output
  -> AgentState           -- ^ Previous conversation state
  -> Text                 -- ^ New user message
  -> IO (Either AgentError (output, AgentState))
continueAgentVerbose client ag prevState newPrompt =
  continueAgentWithCallback client ag prevState newPrompt printEvent
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
    printEvent (EventModelResponse _) = pure ()

    truncateText :: Int -> Text -> Text
    truncateText n t
      | T.length t > n = T.take n t <> "..."
      | otherwise = t

-- | Continue a conversation with a callback for events
continueAgentWithCallback
  :: LLMClient client
  => client
  -> Agent () output
  -> AgentState           -- ^ Previous conversation state
  -> Text                 -- ^ New user message
  -> (AgentEvent -> IO ())
  -> IO (Either AgentError (output, AgentState))
continueAgentWithCallback client ag prevState newPrompt callback = do
  let newState = prevState
        { asMessages = asMessages prevState ++ [Message User [TextPart newPrompt]]
        }
  runLoop () client ag newState 0 callback

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

          -- Check for final_result tool call - if found, parse and return immediately
          case filter (\tc -> tcName tc == resultToolName) toolCalls of
            (resultCall:_) -> do
              -- This is the final result - parse and return
              callback $ EventToolResult resultToolName "final"
              let newState = state
                    { asMessages = asMessages ++
                        [Message Assistant (map ToolUsePart toolCalls)]
                    , asUsage = asUsage <> usage
                    }
              case agentParseOutput (tcInput resultCall) of
                Right output -> pure $ Right (output, newState)
                Left err -> pure $ Left $ ParseError err

            [] -> do
              -- No final_result, execute tools and continue
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

          -- Check for final_result tool call
          case filter (\tc -> tcName tc == resultToolName) toolCalls of
            (resultCall:_) -> do
              -- This is the final result
              callback $ EventToolResult resultToolName "final"
              let newState = state
                    { asMessages = asMessages ++
                        [Message Assistant (TextPart text : map ToolUsePart toolCalls)]
                    , asUsage = asUsage <> usage
                    }
              case agentParseOutput (tcInput resultCall) of
                Right output -> pure $ Right (output, newState)
                Left err -> pure $ Left $ ParseError err

            [] -> do
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

--------------------------------------------------------------------------------
-- Output parsing helpers
--------------------------------------------------------------------------------

-- | Parse JSON from model output (handles string-wrapped JSON and markdown)
parseJsonOutput :: FromJSON a => Value -> Either Text a
parseJsonOutput v = case fromJSON v of
  Success x -> Right x
  Error _ -> case v of
    String t ->
      let cleaned = T.strip $ stripMarkdown t
      in case eitherDecode (LBS.fromStrict $ TE.encodeUtf8 cleaned) of
        Right x -> Right x
        Left err -> Left $ "JSON parse error: " <> T.pack err
    _ -> Left "Expected JSON object or string"
  where
    stripMarkdown t
      | "```json" `T.isPrefixOf` t = T.dropWhile (== '\n') $ T.drop 7 $ T.dropWhileEnd (/= '`') $ T.dropEnd 3 t
      | "```" `T.isPrefixOf` t = T.dropWhile (== '\n') $ T.drop 3 $ T.dropWhileEnd (/= '`') $ T.dropEnd 3 t
      | otherwise = t
