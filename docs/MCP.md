# MCP (Model Context Protocol)

The `Agent.MCP` module provides helpers to connect to MCP servers and adapt
their tools into the agent tool format.

## HTTP MCP example
The `mcp-example` executable reads configuration from environment variables:

```
export MCP_URL="https://example.com/mcp"
export MCP_API_TOKEN="..."
cabal run mcp-example
```

The example sets standard `Authorization` and `x-datarobot-token` headers.
If your MCP server requires different headers, edit `app/MCPExample.hs`.

## Security notes
- Never commit API tokens into source control.
- Prefer `.env` files or secret managers for production usage.
