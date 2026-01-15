{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Agent.Tools
  ( -- * Tool creation helpers
    makeTool,
    simpleSchema,

    -- * Tool execution
    executeTool,
    executeToolCalls,
  )
where

import Agent.Types
import Control.Exception (SomeException, try)
import Data.Aeson
import Data.Aeson.Key (fromText)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

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
