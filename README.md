# haskell-agent

Typed AI agents with tool-calling, model-agnostic clients, and MCP support.

## Highlights
- Typed outputs via JSON Schema and `final_result` tool
- Tool composition with dependency-aware execution
- Anthropic and OpenAI-compatible clients
- MCP (Model Context Protocol) integration over HTTP

## Install
Add to your `.cabal` file:

```
build-depends: haskell-agent
```

Or build locally:

```
cabal build
```

## Quickstart
```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Agent
import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

data Weather = Weather { city :: Text, tempC :: Double }
  deriving (Show, Generic)

instance FromJSON Weather
instance ToSchema Weather

main :: IO ()
main = do
  client <- newClient "<anthropic-api-key>"
  let ag = typedAgent "claude-sonnet-4-20250514" :: Agent () Weather
  runAgent client ag "Weather in Tokyo?" >>= print
```

## Tools
Create tools with `makeTool` or `makeSimpleTool`, then attach them using `withTool`.
Typed agents include an automatic `final_result` tool derived from your output type.

See `docs/USAGE.md` for more detailed examples.

## MCP
The `Agent.MCP` module can connect to HTTP MCP servers and convert tool
definitions into agent tools.

See `docs/MCP.md` for configuration and usage tips.

## Examples
Run the typed agent demo:
```
cabal run agent-example
```

Run the MCP demo (requires env vars):
```
export MCP_URL="https://example.com/mcp"
export MCP_API_TOKEN="..."
cabal run mcp-example
```

## Configuration
Environment variables used by the included examples:
- `MCP_URL` / `MCP_API_TOKEN`: for the MCP example in `app/MCPExample.hs`
- `DATAROBOT_API_TOKEN` / `DATAROBOT_ENDPOINT`: for `newDataRobotClient`

## Development
```
cabal build
cabal haddock
```

## License
MIT
