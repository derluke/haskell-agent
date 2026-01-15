{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | MCP (Model Context Protocol) client implementation
--
-- This module provides MCP clients for both stdio and HTTP transports.
--
-- == Stdio transport (subprocess)
--
-- @
-- server <- connectMCP "npx" ["-y", "\@modelcontextprotocol/server-filesystem", "/tmp"]
-- tools <- mcpTools server
-- agent = typedAgent model `withMCPTools` tools
-- @
--
-- == HTTP transport
--
-- @
-- let config = defaultHttpMCPConfig
--       { httpMCPHeaders = [("Authorization", "Bearer token")]
--       }
-- server <- connectHttpMCP config "https://example.com/mcp"
-- tools <- mcpHttpTools server
-- agent = typedAgent model `withMCPTools` tools
-- @
module Agent.MCP
  ( -- * MCP Server (Stdio)
    MCPServer,
    MCPConfig (..),
    defaultMCPConfig,

    -- * MCP Server (HTTP)
    HttpMCPServer,
    HttpMCPConfig (..),
    defaultHttpMCPConfig,

    -- * Connection (Stdio)
    connectMCP,
    connectMCPWithConfig,
    disconnectMCP,

    -- * Connection (HTTP)
    connectHttpMCP,
    disconnectHttpMCP,

    -- * Tools (Stdio)
    mcpTools,
    mcpToolDefs,
    withMCPServer,
    withMCPTools,

    -- * Tools (HTTP)
    mcpHttpTools,
    mcpHttpToolDefs,
    withHttpMCPServer,

    -- * Low-level API (Stdio)
    mcpListTools,
    mcpCallTool,
    mcpInitialize,

    -- * Low-level API (HTTP)
    mcpHttpListTools,
    mcpHttpCallTool,
    mcpHttpInitialize,

    -- * Errors
    MCPError (..),

    -- * Header helpers
    mkHeader,

    -- * Tool info (for inspection)
    MCPToolInfo (..),
  )
where

import Agent.Types
import Control.Concurrent (MVar, modifyMVar, modifyMVar_, newMVar)
import Control.Exception (Exception, SomeException, throwIO, try)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.CaseInsensitive (mk)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import GHC.Generics (Generic)
import Network.HTTP.Client
  ( Manager,
    RequestBody (..),
    httpLbs,
    method,
    newManager,
    parseRequest,
    requestBody,
    requestHeaders,
    responseBody,
    responseStatus,
  )
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Header (Header, hAccept, hContentType)
import Network.HTTP.Types.Status (statusCode)
import System.IO (BufferMode (..), Handle, hClose, hFlush, hIsEOF, hSetBuffering)
import System.Process
  ( CreateProcess (..),
    ProcessHandle,
    StdStream (..),
    createProcess,
    proc,
    terminateProcess,
    waitForProcess,
  )

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Errors that can occur during MCP operations
data MCPError
  = MCPConnectionError Text
  | MCPProtocolError Text
  | MCPToolError Text Text -- tool name, error message
  | MCPParseError Text
  | MCPTimeout
  deriving (Show, Eq, Generic)

instance Exception MCPError

-- | MCP server configuration
data MCPConfig = MCPConfig
  { -- | Timeout in microseconds (default: 30s)
    mcpTimeout :: Int,
    -- | Client name for initialization
    mcpClientName :: Text,
    -- | Client version for initialization
    mcpClientVersion :: Text
  }
  deriving (Show, Eq)

-- | Default MCP configuration
defaultMCPConfig :: MCPConfig
defaultMCPConfig =
  MCPConfig
    { mcpTimeout = 30000000, -- 30 seconds
      mcpClientName = "haskell-agent",
      mcpClientVersion = "0.1.0"
    }

-- | Handle to an MCP server subprocess (stdio transport)
data MCPServer = MCPServer
  { mcpStdin :: Handle,
    mcpStdout :: Handle,
    mcpProcess :: ProcessHandle,
    mcpRequestId :: MVar Int,
    mcpConfig :: MCPConfig,
    mcpCachedTools :: MVar (Maybe [MCPToolInfo]) -- Cached tool list
  }

--------------------------------------------------------------------------------
-- HTTP Transport Types
--------------------------------------------------------------------------------

-- | Configuration for HTTP-based MCP servers
data HttpMCPConfig = HttpMCPConfig
  { -- | Timeout in microseconds (default: 30s)
    httpMCPTimeout :: Int,
    -- | Client name for initialization
    httpMCPClientName :: Text,
    -- | Client version for initialization
    httpMCPClientVersion :: Text,
    -- | Custom HTTP headers (case-insensitive names)
    httpMCPHeaders :: [Header]
  }

-- | Default HTTP MCP configuration
defaultHttpMCPConfig :: HttpMCPConfig
defaultHttpMCPConfig =
  HttpMCPConfig
    { httpMCPTimeout = 30000000, -- 30 seconds
      httpMCPClientName = "haskell-agent",
      httpMCPClientVersion = "0.1.0",
      httpMCPHeaders = []
    }

-- | Handle to an HTTP-based MCP server
data HttpMCPServer = HttpMCPServer
  { httpMCPUrl :: Text,
    httpMCPManager :: Manager,
    httpMCPRequestId :: MVar Int,
    httpMCPConfig :: HttpMCPConfig,
    httpMCPCachedTools :: MVar (Maybe [MCPToolInfo])
  }

-- | Tool information from MCP server
data MCPToolInfo = MCPToolInfo
  { mtiName :: Text,
    mtiDescription :: Text,
    mtiInputSchema :: Value
  }
  deriving (Show, Eq, Generic)

instance FromJSON MCPToolInfo where
  parseJSON = withObject "MCPToolInfo" $ \o ->
    MCPToolInfo
      <$> o .: "name"
      <*> o .:? "description" .!= ""
      <*> o .:? "inputSchema" .!= emptySchema

-- | Empty object schema as fallback
emptySchema :: Value
emptySchema = object ["type" .= ("object" :: Text), "properties" .= object []]

--------------------------------------------------------------------------------
-- JSON-RPC Protocol
--------------------------------------------------------------------------------

-- | JSON-RPC request
data JsonRpcRequest = JsonRpcRequest
  { jrpcId :: Int,
    jrpcMethod :: Text,
    jrpcParams :: Maybe Value
  }
  deriving (Show, Generic)

instance ToJSON JsonRpcRequest where
  toJSON JsonRpcRequest {..} =
    object
      [ "jsonrpc" .= ("2.0" :: Text),
        "id" .= jrpcId,
        "method" .= jrpcMethod,
        "params" .= jrpcParams
      ]

-- | JSON-RPC response
data JsonRpcResponse = JsonRpcResponse
  { jrpcRespId :: Maybe Int,
    jrpcResult :: Maybe Value,
    jrpcError :: Maybe JsonRpcError
  }
  deriving (Show, Generic)

instance FromJSON JsonRpcResponse where
  parseJSON = withObject "JsonRpcResponse" $ \o ->
    JsonRpcResponse
      <$> o .:? "id"
      <*> o .:? "result"
      <*> o .:? "error"

-- | JSON-RPC error
data JsonRpcError = JsonRpcError
  { jrpcErrCode :: Int,
    jrpcErrMessage :: Text,
    jrpcErrData :: Maybe Value
  }
  deriving (Show, Generic)

instance FromJSON JsonRpcError where
  parseJSON = withObject "JsonRpcError" $ \o ->
    JsonRpcError
      <$> o .: "code"
      <*> o .: "message"
      <*> o .:? "data"

--------------------------------------------------------------------------------
-- Connection
--------------------------------------------------------------------------------

-- | Connect to an MCP server by spawning a subprocess
connectMCP :: FilePath -> [String] -> IO (Either MCPError MCPServer)
connectMCP = connectMCPWithConfig defaultMCPConfig

-- | Connect to an MCP server with custom configuration
connectMCPWithConfig :: MCPConfig -> FilePath -> [String] -> IO (Either MCPError MCPServer)
connectMCPWithConfig config cmd args = do
  result <- try $ do
    let cp =
          (proc cmd args)
            { std_in = CreatePipe,
              std_out = CreatePipe,
              std_err = CreatePipe -- Capture stderr to prevent noise
            }
    (Just stdin, Just stdout, Just _stderr, ph) <- createProcess cp

    -- Set up buffering
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout LineBuffering

    -- Create server handle
    reqId <- newMVar 1
    toolCache <- newMVar Nothing
    let server =
          MCPServer
            { mcpStdin = stdin,
              mcpStdout = stdout,
              mcpProcess = ph,
              mcpRequestId = reqId,
              mcpConfig = config,
              mcpCachedTools = toolCache
            }

    -- Initialize the connection
    initResult <- mcpInitialize server
    case initResult of
      Left err -> do
        disconnectMCP server
        throwIO err
      Right _ -> pure server

  case result of
    Left (e :: SomeException) -> pure $ Left $ MCPConnectionError $ T.pack $ show e
    Right server -> pure $ Right server

-- | Disconnect from an MCP server
disconnectMCP :: MCPServer -> IO ()
disconnectMCP MCPServer {..} = do
  hClose mcpStdin
  hClose mcpStdout
  terminateProcess mcpProcess
  _ <- waitForProcess mcpProcess
  pure ()

--------------------------------------------------------------------------------
-- HTTP Connection
--------------------------------------------------------------------------------

-- | Connect to an HTTP-based MCP server
connectHttpMCP :: HttpMCPConfig -> Text -> IO (Either MCPError HttpMCPServer)
connectHttpMCP config url = do
  result <- try $ do
    manager <- newManager tlsManagerSettings
    reqId <- newMVar 1
    toolCache <- newMVar Nothing
    let server =
          HttpMCPServer
            { httpMCPUrl = url,
              httpMCPManager = manager,
              httpMCPRequestId = reqId,
              httpMCPConfig = config,
              httpMCPCachedTools = toolCache
            }

    -- Initialize the connection
    initResult <- mcpHttpInitialize server
    case initResult of
      Left err -> throwIO err
      Right _ -> pure server

  case result of
    Left (e :: SomeException) -> pure $ Left $ MCPConnectionError $ T.pack $ show e
    Right server -> pure $ Right server

-- | Disconnect from an HTTP MCP server (no-op, but provided for consistency)
disconnectHttpMCP :: HttpMCPServer -> IO ()
disconnectHttpMCP _ = pure ()

--------------------------------------------------------------------------------
-- MCP Protocol Operations (Stdio)
--------------------------------------------------------------------------------

-- | Initialize the MCP connection
mcpInitialize :: MCPServer -> IO (Either MCPError Value)
mcpInitialize server = do
  let params =
        object
          [ "protocolVersion" .= ("2024-11-05" :: Text),
            "capabilities" .= object [],
            "clientInfo"
              .= object
                [ "name" .= mcpClientName (mcpConfig server),
                  "version" .= mcpClientVersion (mcpConfig server)
                ]
          ]
  sendRequest server "initialize" (Just params)

-- | List available tools from the MCP server
mcpListTools :: MCPServer -> IO (Either MCPError [MCPToolInfo])
mcpListTools server = do
  result <- sendRequest server "tools/list" Nothing
  case result of
    Left err -> pure $ Left err
    Right val -> case parseToolList val of
      Nothing -> pure $ Left $ MCPParseError "Failed to parse tools list"
      Just tools -> do
        -- Cache the tools
        modifyMVar_ (mcpCachedTools server) $ \_ -> pure (Just tools)
        pure $ Right tools
  where
    parseToolList :: Value -> Maybe [MCPToolInfo]
    parseToolList = withObject' $ \o -> do
      tools <- KM.lookup "tools" o
      case fromJSON tools of
        Success ts -> Just ts
        Error _ -> Nothing

    withObject' :: (Object -> Maybe a) -> Value -> Maybe a
    withObject' f (Object o) = f o
    withObject' _ _ = Nothing

-- | Call a tool on the MCP server
mcpCallTool :: MCPServer -> Text -> Value -> IO (Either MCPError Value)
mcpCallTool server toolName input = do
  let params =
        object
          [ "name" .= toolName,
            "arguments" .= input
          ]
  result <- sendRequest server "tools/call" (Just params)
  case result of
    Left err -> pure $ Left err
    Right val -> pure $ Right $ extractContent val

-- | Extract content from tool call result
extractContent :: Value -> Value
extractContent val = case val of
  Object o -> case KM.lookup "content" o of
    Just (Array arr) -> case V.toList arr of
      -- Get text from first content block
      (Object c : _) -> case KM.lookup "text" c of
        Just t -> t
        Nothing -> val
      _ -> val
    Just other -> other
    Nothing -> val
  _ -> val

--------------------------------------------------------------------------------
-- MCP Protocol Operations (HTTP)
--------------------------------------------------------------------------------

-- | Initialize the HTTP MCP connection
mcpHttpInitialize :: HttpMCPServer -> IO (Either MCPError Value)
mcpHttpInitialize server = do
  let params =
        object
          [ "protocolVersion" .= ("2024-11-05" :: Text),
            "capabilities" .= object [],
            "clientInfo"
              .= object
                [ "name" .= httpMCPClientName (httpMCPConfig server),
                  "version" .= httpMCPClientVersion (httpMCPConfig server)
                ]
          ]
  sendHttpRequest server "initialize" (Just params)

-- | List available tools from the HTTP MCP server
mcpHttpListTools :: HttpMCPServer -> IO (Either MCPError [MCPToolInfo])
mcpHttpListTools server = do
  result <- sendHttpRequest server "tools/list" Nothing
  case result of
    Left err -> pure $ Left err
    Right val -> case parseToolList val of
      Nothing -> pure $ Left $ MCPParseError "Failed to parse tools list"
      Just tools -> do
        -- Cache the tools
        modifyMVar_ (httpMCPCachedTools server) $ \_ -> pure (Just tools)
        pure $ Right tools
  where
    parseToolList :: Value -> Maybe [MCPToolInfo]
    parseToolList = withObject' $ \o -> do
      tools <- KM.lookup "tools" o
      case fromJSON tools of
        Success ts -> Just ts
        Error _ -> Nothing

    withObject' :: (Object -> Maybe a) -> Value -> Maybe a
    withObject' f (Object o) = f o
    withObject' _ _ = Nothing

-- | Call a tool on the HTTP MCP server
mcpHttpCallTool :: HttpMCPServer -> Text -> Value -> IO (Either MCPError Value)
mcpHttpCallTool server toolName input = do
  let params =
        object
          [ "name" .= toolName,
            "arguments" .= input
          ]
  result <- sendHttpRequest server "tools/call" (Just params)
  case result of
    Left err -> pure $ Left err
    Right val -> pure $ Right $ extractContent val

--------------------------------------------------------------------------------
-- Low-level Communication (Stdio)
--------------------------------------------------------------------------------

-- | Send a JSON-RPC request and wait for response
sendRequest :: MCPServer -> Text -> Maybe Value -> IO (Either MCPError Value)
sendRequest MCPServer {..} method params = do
  -- Get next request ID
  reqId <- modifyMVar mcpRequestId $ \i -> pure (i + 1, i)

  let request = JsonRpcRequest reqId method params
      encoded = LBS.toStrict $ encode request

  -- Send request
  result <- try $ do
    BS.hPut mcpStdin encoded
    BS.hPut mcpStdin "\n"
    hFlush mcpStdin

    -- Read response (simple line-based protocol)
    readResponse mcpStdout

  case result of
    Left (e :: SomeException) -> pure $ Left $ MCPProtocolError $ T.pack $ show e
    Right resp -> case resp of
      Left err -> pure $ Left err
      Right response -> case jrpcError response of
        Just err -> pure $ Left $ MCPProtocolError $ jrpcErrMessage err
        Nothing -> case jrpcResult response of
          Just val -> pure $ Right val
          Nothing -> pure $ Left $ MCPProtocolError "No result in response"

-- | Read a JSON-RPC response from handle
readResponse :: Handle -> IO (Either MCPError JsonRpcResponse)
readResponse h = do
  eof <- hIsEOF h
  if eof
    then pure $ Left $ MCPProtocolError "Connection closed"
    else do
      line <- BS.hGetLine h
      -- Skip empty lines and notifications
      if BS.null line
        then readResponse h
        else case eitherDecode (LBS.fromStrict line) of
          Left err ->
            -- Could be a notification, try reading next line
            if "id" `BS.isInfixOf` line
              then pure $ Left $ MCPParseError $ T.pack err
              else readResponse h
          Right resp ->
            -- Check if it's a notification (no id)
            case jrpcRespId resp of
              Nothing -> readResponse h -- Skip notifications
              Just _ -> pure $ Right resp

--------------------------------------------------------------------------------
-- Low-level Communication (HTTP)
--------------------------------------------------------------------------------

-- | Send a JSON-RPC request over HTTP
sendHttpRequest :: HttpMCPServer -> Text -> Maybe Value -> IO (Either MCPError Value)
sendHttpRequest HttpMCPServer {..} method params = do
  -- Get next request ID
  reqId <- modifyMVar httpMCPRequestId $ \i -> pure (i + 1, i)

  let jsonRpcRequest = JsonRpcRequest reqId method params
      body = encode jsonRpcRequest

  result <- try $ do
    initReq <- parseRequest (T.unpack httpMCPUrl)
    let req =
          initReq
            { method = "POST",
              requestBody = RequestBodyLBS body,
              requestHeaders =
                [ (hContentType, "application/json"),
                  (hAccept, "application/json, text/event-stream")
                ]
                  ++ httpMCPHeaders httpMCPConfig
            }
    response <- httpLbs req httpMCPManager
    let status = statusCode $ responseStatus response
        respBody = responseBody response
    if status >= 200 && status < 300
      then do
        -- Try to parse as SSE first, then fall back to plain JSON
        let jsonBody = extractSSEData respBody
        case eitherDecode jsonBody of
          Left err -> pure $ Left $ MCPParseError $ T.pack err
          Right resp -> case jrpcError resp of
            Just err -> pure $ Left $ MCPProtocolError $ jrpcErrMessage err
            Nothing -> case jrpcResult resp of
              Just val -> pure $ Right val
              Nothing -> pure $ Left $ MCPProtocolError "No result in response"
      else
        pure $
          Left $
            MCPProtocolError $
              "HTTP error "
                <> T.pack (show status)
                <> ": "
                <> TE.decodeUtf8 (LBS.toStrict $ responseBody response)

  case result of
    Left (e :: SomeException) -> pure $ Left $ MCPProtocolError $ T.pack $ show e
    Right r -> pure r

--------------------------------------------------------------------------------
-- Tool Integration (Stdio)
--------------------------------------------------------------------------------

-- | Get Tool definitions from an MCP server (for agent)
mcpToolDefs :: MCPServer -> IO (Either MCPError [ToolDef])
mcpToolDefs server = do
  toolsResult <- mcpListTools server
  case toolsResult of
    Left err -> pure $ Left err
    Right tools -> pure $ Right $ map toToolDef tools
  where
    toToolDef MCPToolInfo {..} =
      ToolDef
        { tdName = mtiName,
          tdDescription = mtiDescription,
          tdInputSchema = mtiInputSchema
        }

-- | Get Tools from an MCP server that can be added to an agent
mcpTools :: MCPServer -> IO (Either MCPError [Tool ()])
mcpTools server = do
  toolsResult <- mcpListTools server
  case toolsResult of
    Left err -> pure $ Left err
    Right tools -> pure $ Right $ map (toTool server) tools
  where
    toTool :: MCPServer -> MCPToolInfo -> Tool ()
    toTool srv MCPToolInfo {..} =
      Tool
        { toolDef =
            ToolDef
              { tdName = mtiName,
                tdDescription = mtiDescription,
                tdInputSchema = mtiInputSchema
              },
          toolExecute = \_ input -> do
            result <- mcpCallTool srv mtiName input
            case result of
              Left err -> pure $ Left $ T.pack $ show err
              Right val -> pure $ Right val
        }

-- | Use an MCP server within a bracket, ensuring cleanup
withMCPServer :: FilePath -> [String] -> (MCPServer -> IO a) -> IO (Either MCPError a)
withMCPServer cmd args action = do
  connectResult <- connectMCP cmd args
  case connectResult of
    Left err -> pure $ Left err
    Right server -> do
      result <- try $ action server
      disconnectMCP server
      case result of
        Left (e :: SomeException) -> pure $ Left $ MCPProtocolError $ T.pack $ show e
        Right a -> pure $ Right a

-- | Add MCP tools to an agent
-- Folds over the tools and adds each one using the provided 'withTool' function
withMCPTools :: Agent deps output -> [Tool ()] -> Agent deps output
withMCPTools ag tools = foldr (flip addMCPTool) ag (map liftMCPTool tools)
  where
    liftMCPTool :: Tool () -> Tool deps
    liftMCPTool tool =
      Tool
        { toolDef = toolDef tool,
          toolExecute = \_ input -> toolExecute tool () input
        }

    addMCPTool :: Agent deps output -> Tool deps -> Agent deps output
    addMCPTool agent tool =
      agent
        { agentTools = Map.insert (tdName $ toolDef tool) tool (agentTools agent)
        }

--------------------------------------------------------------------------------
-- Tool Integration (HTTP)
--------------------------------------------------------------------------------

-- | Get Tool definitions from an HTTP MCP server
mcpHttpToolDefs :: HttpMCPServer -> IO (Either MCPError [ToolDef])
mcpHttpToolDefs server = do
  toolsResult <- mcpHttpListTools server
  case toolsResult of
    Left err -> pure $ Left err
    Right tools -> pure $ Right $ map toToolDef tools
  where
    toToolDef MCPToolInfo {..} =
      ToolDef
        { tdName = mtiName,
          tdDescription = mtiDescription,
          tdInputSchema = mtiInputSchema
        }

-- | Get Tools from an HTTP MCP server that can be added to an agent
mcpHttpTools :: HttpMCPServer -> IO (Either MCPError [Tool ()])
mcpHttpTools server = do
  toolsResult <- mcpHttpListTools server
  case toolsResult of
    Left err -> pure $ Left err
    Right tools -> pure $ Right $ map (toTool server) tools
  where
    toTool :: HttpMCPServer -> MCPToolInfo -> Tool ()
    toTool srv MCPToolInfo {..} =
      Tool
        { toolDef =
            ToolDef
              { tdName = mtiName,
                tdDescription = mtiDescription,
                tdInputSchema = mtiInputSchema
              },
          toolExecute = \_ input -> do
            result <- mcpHttpCallTool srv mtiName input
            case result of
              Left err -> pure $ Left $ T.pack $ show err
              Right val -> pure $ Right val
        }

-- | Use an HTTP MCP server within a bracket, ensuring cleanup
withHttpMCPServer :: HttpMCPConfig -> Text -> (HttpMCPServer -> IO a) -> IO (Either MCPError a)
withHttpMCPServer config url action = do
  connectResult <- connectHttpMCP config url
  case connectResult of
    Left err -> pure $ Left err
    Right server -> do
      result <- try $ action server
      disconnectHttpMCP server
      case result of
        Left (e :: SomeException) -> pure $ Left $ MCPProtocolError $ T.pack $ show e
        Right a -> pure $ Right a

--------------------------------------------------------------------------------
-- Header Helpers
--------------------------------------------------------------------------------

-- | Create an HTTP header from header name and value
-- Example: mkHeader "Authorization" "Bearer token"
mkHeader :: BS.ByteString -> BS.ByteString -> Header
mkHeader name value = (mk name, value)

--------------------------------------------------------------------------------
-- SSE Parsing
--------------------------------------------------------------------------------

-- | Extract JSON data from SSE (Server-Sent Events) response
-- SSE format: "event: message\r\ndata: {...}\r\n\r\n"
-- If not SSE format, return the input unchanged
extractSSEData :: LBS.ByteString -> LBS.ByteString
extractSSEData input =
  let bs = LBS.toStrict input
      -- Find all "data:" lines and concatenate their content
      dataLines = extractDataLines (TE.decodeUtf8 bs)
   in if T.null dataLines
        then input -- Not SSE format, return as-is
        else LBS.fromStrict $ TE.encodeUtf8 dataLines

-- | Extract content after "data:" prefixes from SSE text
extractDataLines :: Text -> Text
extractDataLines txt =
  let lines' = T.lines $ T.replace "\r\n" "\n" $ T.replace "\r" "\n" txt
      dataContents = [T.drop 5 line | line <- lines', "data:" `T.isPrefixOf` line]
   in T.concat dataContents
