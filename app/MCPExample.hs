{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | MCP (Model Context Protocol) Example
--
-- This example demonstrates connecting to an HTTP-based MCP server
-- and using its tools with an agent.
module Main where

import Agent
import Control.Exception (finally)
import Control.Monad (unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Data.Aeson (Value (..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import System.Environment (lookupEnv)

--------------------------------------------------------------------------------
-- Configuration (via environment variables)
--------------------------------------------------------------------------------

data EnvConfig = EnvConfig
  { envMcpUrl :: Text
  , envApiToken :: Text
  }

loadEnvConfig :: IO (Either String EnvConfig)
loadEnvConfig = do
  maybeUrl <- lookupEnv "MCP_URL"
  maybeToken <- lookupEnv "MCP_API_TOKEN"
  case (maybeUrl, maybeToken) of
    (Just url, Just token) ->
      pure $ Right EnvConfig
        { envMcpUrl = T.pack url
        , envApiToken = T.pack token
        }
    (Nothing, _) -> pure $ Left "MCP_URL not set"
    (_, Nothing) -> pure $ Left "MCP_API_TOKEN not set"

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "Haskell Agent Framework - MCP Example"
  putStrLn "======================================"
  putStrLn ""
  loadEnvConfig >>= \case
    Left err -> do
      putStrLn $ "Missing configuration: " ++ err
      putStrLn "Set MCP_URL and MCP_API_TOKEN, then re-run the example."
    Right EnvConfig{..} -> do
      putStrLn $ "Connecting to: " ++ T.unpack envMcpUrl
      putStrLn ""
      let wrenConfig =
            defaultHttpMCPConfig
              { httpMCPHeaders =
                  [ mkHeader "Authorization" ("Bearer " <> TE.encodeUtf8 envApiToken)
                  , mkHeader "x-datarobot-token" ("Bearer " <> TE.encodeUtf8 envApiToken)
                  ]
              }
      connectHttpMCP wrenConfig envMcpUrl >>= \case
        Left err -> putStrLn $ "Failed to connect: " ++ show err
        Right mcpServer ->
          finally (runMcpFlow mcpServer) $ do
            disconnectHttpMCP mcpServer
            putStrLn ""
            putStrLn "Disconnected from MCP server."
  where
    systemPrompt =
      "You are a helpful assistant with access to MCP tools. \
      \Use the available tools to help answer questions."

    parseText (String t) = Right t
    parseText _ = Left "Expected text response"

    runMcpFlow mcpServer = do
      putStrLn "Connected to MCP server!"
      putStrLn ""
      result <- runExceptT $ do
        tools <- withError "Failed to list tools" (mcpHttpListTools mcpServer)
        lift $ do
          putStrLn $ "Available tools (" ++ show (length tools) ++ "):"
          mapM_ printToolInfo tools
          putStrLn ""

        mcpAgentTools <- withError "Failed to get tools" (mcpHttpTools mcpServer)
        lift $ do
          putStrLn "Creating agent with MCP tools..."
          putStrLn ""

        client <- withTextError "Error creating client" newDataRobotClient
        let mcpAgent =
              agent "azure/gpt-4o" systemPrompt parseText
                `withMCPTools` mcpAgentTools
        lift $ do
          putStrLn "Starting chat. Type :quit or an empty line to exit."
          putStrLn ""
          chatLoop client mcpAgent
      either putStrLn (const (pure ())) result

    chatLoop client mcpAgent = go Nothing
      where
        go mState = do
          userInput <- promptInput
          unless (shouldQuit userInput) $ do
            result <- maybe
              (runAgentVerbose client mcpAgent userInput)
              (\state -> continueAgentVerbose client mcpAgent state userInput)
              mState
            case result of
              Left err -> putStrLn $ "Agent error: " ++ show err
              Right (response, newState) -> do
                printResponse response
                go (Just newState)

    promptInput = do
      putStr "You: "
      TIO.getLine

    printResponse response = do
      putStrLn ""
      putStrLn "=== Agent Response ==="
      TIO.putStrLn response
      putStrLn ""

    shouldQuit input =
      T.strip input == "" || T.toLower (T.strip input) == ":quit"

    printToolInfo tool = do
      TIO.putStrLn $ "  - " <> mtiName tool
      let desc = mtiDescription tool
      TIO.putStrLn $ "    " <> if T.null desc then "(no description)" else desc

    withError label action =
      ExceptT $ action >>= \case
        Left err -> pure $ Left (label <> ": " <> show err)
        Right value -> pure $ Right value

    withTextError label action =
      ExceptT $ action >>= \case
        Left err -> pure $ Left (label <> ": " <> T.unpack err)
        Right value -> pure $ Right value
