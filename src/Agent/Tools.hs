{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Agent.Tools
  ( -- * Tool creation helpers
    makeTool,
    makeSimpleTool,
    liftTool,
    simpleSchema,

    -- * Result tool (for typed outputs)
    resultTool,
    resultToolName,

    -- * Tool execution
    executeTool,
    executeToolCalls,
  )
where

import Agent.Types
import Control.Exception (SomeException, try)
import Data.Aeson
import Data.Aeson.Key (fromText)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- | Create a simple JSON schema for a tool
simpleSchema :: [(Text, Text, Text)] -> Value
simpleSchema props =
  object
    [ "type" .= ("object" :: Text),
      "properties"
        .= object
          [ ( fromText name,
              object
                [ "type" .= typ,
                  "description" .= desc
                ]
            )
            | (name, typ, desc) <- props
          ],
      "required" .= [name | (name, _, _) <- props]
    ]

-- | Helper to create a tool
makeTool ::
  -- | Name
  Text ->
  -- | Description
  Text ->
  -- | Input schema
  Value ->
  -- | Execution function
  (deps -> Value -> IO (Either Text Value)) ->
  Tool deps
makeTool name desc schema execute =
  Tool
    { toolDef = ToolDef name desc schema,
      toolExecute = execute
    }

-- | Helper to create a simple tool that ignores dependencies
makeSimpleTool ::
  -- | Name
  Text ->
  -- | Description
  Text ->
  -- | Input schema
  Value ->
  -- | Execution function (no deps)
  (Value -> IO (Either Text Value)) ->
  Tool deps
makeSimpleTool name desc schema execute =
  Tool
    { toolDef = ToolDef name desc schema,
      toolExecute = \_ input -> execute input
    }

-- | Lift a Tool () to work with any deps type
liftTool :: Tool () -> Tool deps
liftTool tool =
  Tool
    { toolDef = toolDef tool,
      toolExecute = \_ input -> toolExecute tool () input
    }

-- | The name of the result tool
resultToolName :: Text
resultToolName = "final_result"

-- | Create a result tool from a type's schema.
-- When the model calls this tool, the agent returns the parsed output.
-- The tool execution just echoes back the input as JSON for parsing.
resultTool :: forall output deps. (ToSchema output) => Proxy output -> Tool deps
resultTool proxy =
  Tool
    { toolDef =
        ToolDef
          { tdName = resultToolName,
            tdDescription = "Return the final result. Call this when you have the answer.",
            tdInputSchema = toSchema proxy
          },
      toolExecute = \_ input ->
        -- Just echo back the input as JSON string for the agent to parse
        pure $ Right $ String $ TE.decodeUtf8 $ LBS.toStrict $ encode input
    }

-- | Execute a single tool
executeTool ::
  Map.Map Text (Tool deps) ->
  deps ->
  ToolCall ->
  IO ToolResult
executeTool tools deps ToolCall {..} =
  case Map.lookup tcName tools of
    Nothing ->
      pure
        ToolResult
          { trToolUseId = tcId,
            trContent = "Unknown tool: " <> tcName,
            trIsError = True
          }
    Just tool -> do
      result <- try $ toolExecute tool deps tcInput
      case result of
        Left (e :: SomeException) ->
          pure
            ToolResult
              { trToolUseId = tcId,
                trContent = "Tool execution error: " <> T.pack (show e),
                trIsError = True
              }
        Right (Left err) ->
          pure
            ToolResult
              { trToolUseId = tcId,
                trContent = err,
                trIsError = True
              }
        Right (Right val) ->
          pure
            ToolResult
              { trToolUseId = tcId,
                trContent = case val of
                  String t -> t
                  _ -> T.pack $ show val,
                trIsError = False
              }

-- | Execute multiple tool calls
executeToolCalls ::
  Map.Map Text (Tool deps) ->
  deps ->
  [ToolCall] ->
  IO [ToolResult]
executeToolCalls tools deps = mapM (executeTool tools deps)
