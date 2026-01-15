{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Agent.Types
  ( -- * Messages
    Role (..),
    MessagePart (..),
    Message (..),
    ToolCall (..),
    ToolResult (..),

    -- * Tools
    Tool (..),
    ToolDef (..),

    -- * Agent
    Agent (..),
    AgentConfig (..),
    RunContext (..),
    AgentState (..),
    AgentError (..),

    -- * Model Response
    ModelResponse (..),
    Usage (..),
  )
where

import Data.Aeson
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Message roles in conversation
data Role = System | User | Assistant
  deriving (Show, Eq, Generic)

instance ToJSON Role where
  toJSON System = "system"
  toJSON User = "user"
  toJSON Assistant = "assistant"

instance FromJSON Role where
  parseJSON = withText "Role" $ \t -> case t of
    "system" -> pure System
    "user" -> pure User
    "assistant" -> pure Assistant
    _ -> fail "Invalid role"

-- | Tool call from model
data ToolCall = ToolCall
  { tcId :: Text,
    tcName :: Text,
    tcInput :: Value -- JSON input
  }
  deriving (Show, Eq, Generic)

instance ToJSON ToolCall

instance FromJSON ToolCall

-- | Result of tool execution
data ToolResult = ToolResult
  { trToolUseId :: Text,
    trContent :: Text,
    trIsError :: Bool
  }
  deriving (Show, Eq, Generic)

-- | Parts of a message
data MessagePart
  = TextPart Text
  | ToolUsePart ToolCall
  | ToolResultPart ToolResult
  deriving (Show, Eq)

-- | A message in the conversation
data Message = Message
  { msgRole :: Role,
    msgContent :: [MessagePart]
  }
  deriving (Show, Eq)

-- | Tool definition for the model
data ToolDef = ToolDef
  { tdName :: Text,
    tdDescription :: Text,
    tdInputSchema :: Value -- JSON Schema
  }
  deriving (Show, Eq, Generic)

instance ToJSON ToolDef where
  toJSON td =
    object
      [ "name" .= tdName td,
        "description" .= tdDescription td,
        "input_schema" .= tdInputSchema td
      ]

-- | A tool that can be executed
data Tool deps = Tool
  { toolDef :: ToolDef,
    toolExecute :: deps -> Value -> IO (Either Text Value)
  }

-- | Run context available to tools
data RunContext deps = RunContext
  { rcDeps :: deps,
    rcHistory :: [Message]
  }

-- | Agent state during execution
data AgentState = AgentState
  { asMessages :: [Message],
    asUsage :: Usage
  }
  deriving (Show)

-- | Token usage tracking
data Usage = Usage
  { usageInput :: Int,
    usageOutput :: Int
  }
  deriving (Show, Eq, Generic)

instance ToJSON Usage

instance FromJSON Usage

instance Semigroup Usage where
  u1 <> u2 =
    Usage
      { usageInput = usageInput u1 + usageInput u2,
        usageOutput = usageOutput u1 + usageOutput u2
      }

instance Monoid Usage where
  mempty = Usage 0 0

-- | Agent configuration
data AgentConfig = AgentConfig
  { acModel :: Text, -- e.g. "claude-sonnet-4-20250514"
    acMaxTokens :: Int,
    acSystemPrompt :: Text,
    acMaxRetries :: Int
  }
  deriving (Show)

-- | The Agent type
data Agent deps output = Agent
  { agentConfig :: AgentConfig,
    agentTools :: Map.Map Text (Tool deps),
    agentParseOutput :: Value -> Either Text output
  }

-- | Model response from API
data ModelResponse
  = TextResponse Text Usage
  | ToolUseResponse [ToolCall] Usage
  | MixedResponse Text [ToolCall] Usage
  deriving (Show)

-- | Errors that can occur
data AgentError
  = ApiError Text
  | ToolError Text Text -- tool name, error message
  | ParseError Text
  | MaxRetriesExceeded
  deriving (Show, Eq)
