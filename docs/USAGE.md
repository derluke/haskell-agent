# Usage

This document covers the core workflow for building typed agents and tools.

## Typed agents
Typed agents use your output type to generate a JSON Schema and ensure that the
model returns a `final_result` tool call matching that schema.

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Agent
import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

data Answer = Answer { value :: Text }
  deriving (Show, Generic)

instance FromJSON Answer
instance ToSchema Answer

answerAgent :: Agent () Answer
answerAgent = typedAgent "gpt-4o-mini"
```

## Tools
Define tools with JSON schema inputs, then attach them to agents:

```haskell
import Agent
import Data.Aeson (Value, object, (.=))

echoTool :: Tool ()
echoTool =
  makeSimpleTool
    "echo"
    "Return the input text"
    (simpleSchema [("text", "string", "Text to echo")])
    (\input -> pure (Right input))
```

Use `withTool` to attach tools:

```haskell
agentWithEcho :: Agent () Answer
agentWithEcho = typedAgent "gpt-4o-mini" `withTool` echoTool
```

## Chaining with dependencies
Use `typedAgentWithDeps` when the output depends on prior steps:

```haskell
step2 :: Agent Answer Answer
step2 = typedAgentWithDeps "gpt-4o-mini"
```

Run with `runAgentWithDeps` and pass the dependencies from earlier steps.
