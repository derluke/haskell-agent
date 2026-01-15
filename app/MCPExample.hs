{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

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

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | Wren MCP server URL
wrenMCPUrl :: Text
wrenMCPUrl = "https://app.datarobot.com/api/v2/deployments/693bfc1fedd8925c19620fc7/directAccess/mcp"

-- | API token for authentication
apiToken :: Text
apiToken = "NjhlZTE0NTIzMzYyNzE2M2FjMGY4NTgxOnhUcG9FRGt1UW55Yy90VGlnaUR1QVVmUXUrdTVqaFBNQWtwWEJiRTQrRnM9"

-- | Create the MCP config with authentication headers
wrenConfig :: HttpMCPConfig
wrenConfig =
  defaultHttpMCPConfig
    { httpMCPHeaders =
        [ mkHeader "Authorization" ("Bearer " <> TE.encodeUtf8 apiToken),
          mkHeader "x-datarobot-token" ("Bearer " <> TE.encodeUtf8 apiToken)
        ]
    }

--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "Haskell Agent Framework - MCP Example"
  putStrLn "======================================"
  putStrLn ""
  putStrLn $ "Connecting to: " ++ T.unpack wrenMCPUrl
  putStrLn ""

  connectHttpMCP wrenConfig wrenMCPUrl >>= \case
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
