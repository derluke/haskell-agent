{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}

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

    -- * Schema generation
    ToSchema (..),
  )
where

import Data.Aeson
import Data.Aeson.Key (fromText)
import Data.Char (toLower)
import Data.Kind (Type)
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics

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

--------------------------------------------------------------------------------
-- Schema Generation (with Generic derivation)
--------------------------------------------------------------------------------

-- | Typeclass for generating JSON schemas from Haskell types.
-- This enables automatic result tool generation based on the output type.
--
-- For simple record types, you can derive this automatically:
--
-- @
-- data MyType = MyType { field1 :: Text, field2 :: Int }
--   deriving (Generic)
--
-- instance ToSchema MyType  -- Uses generic default
-- @
class ToSchema a where
  -- | Generate a JSON schema for this type
  toSchema :: proxy a -> Value
  default toSchema :: (Generic a, GToSchema (Rep a)) => proxy a -> Value
  toSchema _ = gToSchema (Proxy :: Proxy (Rep a))

  -- | Get the type name (used in tool description)
  typeName :: proxy a -> Text
  default typeName :: (Generic a, GToSchema (Rep a)) => proxy a -> Text
  typeName _ = gTypeName (Proxy :: Proxy (Rep a))

-- | Generic schema generation for record types
class GToSchema (f :: Type -> Type) where
  gToSchema :: proxy f -> Value
  gTypeName :: proxy f -> Text

-- | Datatype metadata (D1)
instance (Datatype d, GToSchemaFields f) => GToSchema (D1 d f) where
  gToSchema _ =
    let fields = gToSchemaFields (Proxy :: Proxy f)
        fieldNames = map fst fields
    in object
         [ "type" .= ("object" :: Text)
         , "properties" .= object [(fromText k, v) | (k, v) <- fields]
         , "required" .= fieldNames
         ]
  gTypeName _ = T.pack $ datatypeName (undefined :: D1 d f ())

-- | Fields within a constructor
class GToSchemaFields (f :: Type -> Type) where
  gToSchemaFields :: proxy f -> [(Text, Value)]

-- | Constructor metadata (C1) - unwrap and get fields
instance GToSchemaFields f => GToSchemaFields (C1 c f) where
  gToSchemaFields _ = gToSchemaFields (Proxy :: Proxy f)

-- | Product of fields (record with multiple fields)
instance (GToSchemaFields a, GToSchemaFields b) => GToSchemaFields (a :*: b) where
  gToSchemaFields _ = gToSchemaFields (Proxy :: Proxy a) ++ gToSchemaFields (Proxy :: Proxy b)

-- | Single field (S1) with selector name
instance (Selector s, GToSchemaType a) => GToSchemaFields (S1 s a) where
  gToSchemaFields _ =
    let name = T.pack $ selName (undefined :: S1 s a ())
        -- Strip common prefixes like "wi", "ct", "wr" from field names
        cleanName = stripFieldPrefix name
    in [(cleanName, gToSchemaType (Proxy :: Proxy a))]

-- | Strip common record field prefixes and convert to snake_case
-- e.g., wiTemperatureC -> temperature_c, ctCelsius -> celsius
stripFieldPrefix :: Text -> Text
stripFieldPrefix name
  | T.length name > 2 && T.all (`elem` ['a'..'z']) (T.take 2 name) =
      let rest = T.drop 2 name
      in if not (T.null rest) && T.head rest `elem` ['A'..'Z']
         then toSnakeCase $ T.cons (toLower $ T.head rest) (T.tail rest)
         else name
  | otherwise = name

-- | Convert camelCase to snake_case
-- e.g., temperatureC -> temperature_c, feelsLike -> feels_like
toSnakeCase :: Text -> Text
toSnakeCase = T.pack . go . T.unpack
  where
    go [] = []
    go (c:cs)
      | c `elem` ['A'..'Z'] = '_' : toLower c : go cs
      | otherwise = c : go cs

-- | Type-level schema for field types
class GToSchemaType (f :: Type -> Type) where
  gToSchemaType :: proxy f -> Value

-- | Unwrap Rec0 (the actual field type)
instance ToSchemaType a => GToSchemaType (K1 i a) where
  gToSchemaType _ = toSchemaType (Proxy :: Proxy a)

-- | Schema for primitive/nested types
class ToSchemaType a where
  toSchemaType :: proxy a -> Value

instance ToSchemaType Text where
  toSchemaType _ = object ["type" .= ("string" :: Text)]

instance ToSchemaType String where
  toSchemaType _ = object ["type" .= ("string" :: Text)]

instance ToSchemaType Int where
  toSchemaType _ = object ["type" .= ("integer" :: Text)]

instance ToSchemaType Integer where
  toSchemaType _ = object ["type" .= ("integer" :: Text)]

instance ToSchemaType Double where
  toSchemaType _ = object ["type" .= ("number" :: Text)]

instance ToSchemaType Float where
  toSchemaType _ = object ["type" .= ("number" :: Text)]

instance ToSchemaType Bool where
  toSchemaType _ = object ["type" .= ("boolean" :: Text)]

-- | Nested objects use their ToSchema instance
instance {-# OVERLAPPABLE #-} ToSchema a => ToSchemaType a where
  toSchemaType _ = toSchema (Proxy :: Proxy a)
