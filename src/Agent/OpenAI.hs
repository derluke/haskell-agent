{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module Agent.OpenAI
  ( OpenAIClient(..)
  , newOpenAIClient
  , newDataRobotClient
  , sendOpenAIRequest
  ) where

import Agent.Types

import Data.Aeson
import Data.Aeson.KeyMap (toList)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import System.Environment (lookupEnv)

-- | OpenAI-compatible API client
data OpenAIClient = OpenAIClient
  { oaiApiKey :: Text
  , oaiBaseUrl :: Text
  , oaiManager :: Manager
  }

-- | Create a new OpenAI client
newOpenAIClient :: Text -> Text -> IO OpenAIClient
newOpenAIClient apiKey baseUrl = do
  manager <- newManager tlsManagerSettings
  pure OpenAIClient
    { oaiApiKey = apiKey
    , oaiBaseUrl = baseUrl
    , oaiManager = manager
    }

-- | Create a client from DATAROBOT_API_TOKEN and DATAROBOT_ENDPOINT env vars
newDataRobotClient :: IO (Either Text OpenAIClient)
newDataRobotClient = do
  maybeToken <- lookupEnv "DATAROBOT_API_TOKEN"
  maybeEndpoint <- lookupEnv "DATAROBOT_ENDPOINT"
  case (maybeToken, maybeEndpoint) of
    (Just token, Just endpoint) -> do
      let baseUrl = T.pack endpoint <> "/genai/llmgw/"
      client <- newOpenAIClient (T.pack token) baseUrl
      pure $ Right client
    (Nothing, _) -> pure $ Left "DATAROBOT_API_TOKEN not set"
    (_, Nothing) -> pure $ Left "DATAROBOT_ENDPOINT not set"

-- | Convert our messages to OpenAI API format
-- OpenAI has different message structure:
-- - User/assistant messages have role + content
-- - Assistant tool calls have role + tool_calls array
-- - Tool results are separate messages with role "tool"
messagesToJson :: Text -> [Message] -> [Value]
messagesToJson systemPrompt msgs = systemMsg : concatMap messageToJson msgs
  where
    systemMsg = object
      [ "role" .= ("system" :: Text)
      , "content" .= systemPrompt
      ]

    messageToJson :: Message -> [Value]
    messageToJson Message{..} = case msgRole of
      User -> userMessages msgContent
      Assistant -> assistantMessages msgContent
      System -> [object ["role" .= ("system" :: Text), "content" .= contentText msgContent]]

    -- User messages: either plain text or tool results
    userMessages :: [MessagePart] -> [Value]
    userMessages parts
      | all isToolResult parts = map toolResultToMsg parts
      | otherwise = [object
          [ "role" .= ("user" :: Text)
          , "content" .= contentText parts
          ]]

    -- Assistant messages: either plain text or tool calls
    assistantMessages :: [MessagePart] -> [Value]
    assistantMessages parts =
      let texts = [t | TextPart t <- parts]
          toolCalls = [tc | ToolUsePart tc <- parts]
          textContent = if null texts then Null else String (T.concat texts)
      in if null toolCalls
         then [object
           [ "role" .= ("assistant" :: Text)
           , "content" .= textContent
           ]]
         else [object $
           [ "role" .= ("assistant" :: Text)
           , "tool_calls" .= map toolCallToJson toolCalls
           ] ++ if null texts then [] else ["content" .= T.concat texts]]

    toolCallToJson :: ToolCall -> Value
    toolCallToJson ToolCall{..} = object
      [ "id" .= tcId
      , "type" .= ("function" :: Text)
      , "function" .= object
          [ "name" .= tcName
          , "arguments" .= encodeToText tcInput
          ]
      ]

    encodeToText :: Value -> Text
    encodeToText v = TE.decodeUtf8 $ LBS.toStrict $ encode v

    toolResultToMsg :: MessagePart -> Value
    toolResultToMsg (ToolResultPart ToolResult{..}) = object
      [ "role" .= ("tool" :: Text)
      , "tool_call_id" .= trToolUseId
      , "content" .= trContent
      ]
    toolResultToMsg _ = Null  -- shouldn't happen

    isToolResult :: MessagePart -> Bool
    isToolResult (ToolResultPart _) = True
    isToolResult _ = False

    contentText :: [MessagePart] -> Text
    contentText parts = T.concat [t | TextPart t <- parts]

-- | Convert tool definitions to OpenAI format
toolsToJson :: [ToolDef] -> [Value]
toolsToJson = map toOpenAITool
  where
    toOpenAITool ToolDef{..} = object
      [ "type" .= ("function" :: Text)
      , "function" .= object
          [ "name" .= tdName
          , "description" .= tdDescription
          , "parameters" .= tdInputSchema
          ]
      ]

-- | Send a request to an OpenAI-compatible API
sendOpenAIRequest
  :: OpenAIClient
  -> AgentConfig
  -> [Message]
  -> [ToolDef]
  -> IO (Either AgentError ModelResponse)
sendOpenAIRequest OpenAIClient{..} AgentConfig{..} messages tools = do
  let body = object $
        [ "model" .= acModel
        , "max_tokens" .= acMaxTokens
        , "messages" .= messagesToJson acSystemPrompt messages
        ] ++ if null tools then [] else [ "tools" .= toolsToJson tools ]

  let url = T.unpack oaiBaseUrl <> "chat/completions"
  initialRequest <- parseRequest url
  let request = initialRequest
        { method = "POST"
        , requestHeaders =
            [ ("Content-Type", "application/json")
            , ("Authorization", "Bearer " <> TE.encodeUtf8 oaiApiKey)
            ]
        , requestBody = RequestBodyLBS (encode body)
        }

  response <- httpLbs request oaiManager
  let status = statusCode (responseStatus response)
      respBody = responseBody response

  if status >= 200 && status < 300
    then parseResponse respBody
    else pure $ Left $ ApiError $
      "API error " <> T.pack (show status) <> ": " <>
      TE.decodeUtf8 (LBS.toStrict respBody)

-- | Parse the API response
parseResponse :: LBS.ByteString -> IO (Either AgentError ModelResponse)
parseResponse body = case eitherDecode body of
  Left err -> pure $ Left $ ParseError $ T.pack err
  Right val -> pure $ parseModelResponse val

-- | Parse the model response JSON (OpenAI format)
parseModelResponse :: Value -> Either AgentError ModelResponse
parseModelResponse = \case
  Object obj -> do
    -- Get usage
    usage <- case lookup "usage" (kvList obj) of
      Just (Object uObj) -> do
        inp <- maybe (Right 0) parseNum $ lookup "prompt_tokens" (kvList uObj)
        out <- maybe (Right 0) parseNum $ lookup "completion_tokens" (kvList uObj)
        Right $ Usage inp out
      _ -> Right mempty

    -- Get choices[0].message
    choices <- case lookup "choices" (kvList obj) of
      Just (Array arr) | not (null arr) -> Right $ head $ foldr (:) [] arr
      _ -> Left $ ParseError "missing choices"

    case choices of
      Object choiceObj -> case lookup "message" (kvList choiceObj) of
        Just (Object msgObj) -> parseMessage msgObj usage
        _ -> Left $ ParseError "missing message in choice"
      _ -> Left $ ParseError "invalid choice format"

  _ -> Left $ ParseError "expected object response"

  where
    kvList o = map (\(k, v) -> (k, v)) $ toList o

    parseNum :: Value -> Either AgentError Int
    parseNum (Number n) = Right $ round n
    parseNum _ = Left $ ParseError "expected number"

    parseMessage :: Object -> Usage -> Either AgentError ModelResponse
    parseMessage msgObj usage = do
      -- Check for tool_calls first
      case lookup "tool_calls" (kvList msgObj) of
        Just (Array toolCalls) | not (null toolCalls) -> do
          tcs <- parseToolCalls $ foldr (:) [] toolCalls
          -- Also get any text content
          case lookup "content" (kvList msgObj) of
            Just (String t) | not (T.null t) -> Right $ MixedResponse t tcs usage
            _ -> Right $ ToolUseResponse tcs usage
        _ -> do
          -- Just text response
          case lookup "content" (kvList msgObj) of
            Just (String t) -> Right $ TextResponse t usage
            Just Null -> Right $ TextResponse "" usage
            _ -> Left $ ParseError "missing content in message"

    parseToolCalls :: [Value] -> Either AgentError [ToolCall]
    parseToolCalls = mapM parseToolCall

    parseToolCall :: Value -> Either AgentError ToolCall
    parseToolCall (Object tcObj) = do
      tcId <- case lookup "id" (kvList tcObj) of
        Just (String i) -> Right i
        _ -> Left $ ParseError "missing tool call id"

      case lookup "function" (kvList tcObj) of
        Just (Object fnObj) -> do
          tcName <- case lookup "name" (kvList fnObj) of
            Just (String n) -> Right n
            _ -> Left $ ParseError "missing function name"
          tcInput <- case lookup "arguments" (kvList fnObj) of
            Just (String args) -> case eitherDecode (LBS.fromStrict $ TE.encodeUtf8 args) of
              Right v -> Right v
              Left _ -> Right $ String args  -- Keep as string if not valid JSON
            Just v -> Right v
            _ -> Left $ ParseError "missing function arguments"
          Right ToolCall{..}
        _ -> Left $ ParseError "missing function in tool call"
    parseToolCall _ = Left $ ParseError "invalid tool call format"
