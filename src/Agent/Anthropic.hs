{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Agent.Anthropic
  ( AnthropicClient (..),
    newClient,
    sendRequest,
  )
where

import Agent.Types
import Control.Monad (forM)
import Data.Aeson
import Data.Aeson.KeyMap (toList)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)

-- | Anthropic API client
data AnthropicClient = AnthropicClient
  { clientApiKey :: Text,
    clientManager :: Manager
  }

-- | Create a new Anthropic client
newClient :: Text -> IO AnthropicClient
newClient apiKey = do
  manager <- newManager tlsManagerSettings
  pure
    AnthropicClient
      { clientApiKey = apiKey,
        clientManager = manager
      }

-- | Convert our messages to Anthropic API format
messagesToJson :: [Message] -> [Value]
messagesToJson = map messageToJson
  where
    messageToJson :: Message -> Value
    messageToJson Message {..} =
      object
        [ "role" .= msgRole,
          "content" .= map partToJson msgContent
        ]

    partToJson :: MessagePart -> Value
    partToJson (TextPart t) =
      object
        [ "type" .= ("text" :: Text),
          "text" .= t
        ]
    partToJson (ToolUsePart ToolCall {..}) =
      object
        [ "type" .= ("tool_use" :: Text),
          "id" .= tcId,
          "name" .= tcName,
          "input" .= tcInput
        ]
    partToJson (ToolResultPart ToolResult {..}) =
      object
        [ "type" .= ("tool_result" :: Text),
          "tool_use_id" .= trToolUseId,
          "content" .= trContent,
          "is_error" .= trIsError
        ]

-- | Convert tool definitions to Anthropic format
toolsToJson :: [ToolDef] -> [Value]
toolsToJson = map toJSON

-- | Send a request to the Anthropic API
sendRequest ::
  AnthropicClient ->
  AgentConfig ->
  [Message] ->
  [ToolDef] ->
  IO (Either AgentError ModelResponse)
sendRequest AnthropicClient {..} AgentConfig {..} messages tools = do
  let body =
        object $
          [ "model" .= acModel,
            "max_tokens" .= acMaxTokens,
            "system" .= acSystemPrompt,
            "messages" .= messagesToJson messages
          ]
            ++ if null tools then [] else ["tools" .= toolsToJson tools]

  initialRequest <- parseRequest "https://api.anthropic.com/v1/messages"
  let request =
        initialRequest
          { method = "POST",
            requestHeaders =
              [ ("Content-Type", "application/json"),
                ("x-api-key", TE.encodeUtf8 acApiKey),
                ("anthropic-version", "2023-06-01")
              ],
            requestBody = RequestBodyLBS (encode body)
          }
        where
          acApiKey = clientApiKey

  response <- httpLbs request clientManager
  let status = statusCode (responseStatus response)
      respBody = responseBody response

  if status >= 200 && status < 300
    then parseResponse respBody
    else
      pure $
        Left $
          ApiError $
            "API error "
              <> T.pack (show status)
              <> ": "
              <> TE.decodeUtf8 (LBS.toStrict respBody)

-- | Parse the API response
parseResponse :: LBS.ByteString -> IO (Either AgentError ModelResponse)
parseResponse body = case eitherDecode body of
  Left err -> pure $ Left $ ParseError $ T.pack err
  Right val -> pure $ parseModelResponse val

-- | Parse the model response JSON
parseModelResponse :: Value -> Either AgentError ModelResponse
parseModelResponse = \case
  Object obj -> do
    content <-
      maybe (Left $ ParseError "missing content") Right $
        lookup "content" (map (\(k, v) -> (k, v)) $ toList obj)

    usage <- case lookup "usage" (map (\(k, v) -> (k, v)) $ toList obj) of
      Just (Object uObj) -> do
        inp <-
          maybe (Left $ ParseError "missing input_tokens") parseNum $
            lookup "input_tokens" (map (\(k, v) -> (k, v)) $ toList uObj)
        out <-
          maybe (Left $ ParseError "missing output_tokens") parseNum $
            lookup "output_tokens" (map (\(k, v) -> (k, v)) $ toList uObj)
        Right $ Usage inp out
      _ -> Right mempty

    parseContent content usage
  _ -> Left $ ParseError "expected object response"
  where
    parseNum :: Value -> Either AgentError Int
    parseNum (Number n) = Right $ round n
    parseNum _ = Left $ ParseError "expected number"

    parseContent :: Value -> Usage -> Either AgentError ModelResponse
    parseContent (Array arr) usage = do
      let items = foldr (:) [] arr
      (texts, toolCalls) <- collectParts items
      case (texts, toolCalls) of
        ([], []) -> Left $ ParseError "empty content"
        (ts, []) -> Right $ TextResponse (T.unlines ts) usage
        ([], tcs) -> Right $ ToolUseResponse tcs usage
        (ts, tcs) -> Right $ MixedResponse (T.unlines ts) tcs usage
    parseContent _ _ = Left $ ParseError "expected array content"

    collectParts :: [Value] -> Either AgentError ([Text], [ToolCall])
    collectParts items = do
      results <- forM items parsePart
      let texts = [t | Left t <- results]
          tools = [tc | Right tc <- results]
      Right (texts, tools)

    parsePart :: Value -> Either AgentError (Either Text ToolCall)
    parsePart (Object obj) = case lookup "type" (map (\(k, v) -> (k, v)) $ toList obj) of
      Just (String "text") ->
        case lookup "text" (map (\(k, v) -> (k, v)) $ toList obj) of
          Just (String t) -> Right $ Left t
          _ -> Left $ ParseError "missing text"
      Just (String "tool_use") -> do
        tcId <- case lookup "id" (map (\(k, v) -> (k, v)) $ toList obj) of
          Just (String i) -> Right i
          _ -> Left $ ParseError "missing tool id"
        tcName <- case lookup "name" (map (\(k, v) -> (k, v)) $ toList obj) of
          Just (String n) -> Right n
          _ -> Left $ ParseError "missing tool name"
        tcInput <- case lookup "input" (map (\(k, v) -> (k, v)) $ toList obj) of
          Just v -> Right v
          _ -> Left $ ParseError "missing tool input"
        Right $ Right ToolCall {..}
      _ -> Left $ ParseError "unknown content type"
    parsePart _ = Left $ ParseError "expected object in content"
