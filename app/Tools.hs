{-# LANGUAGE OverloadedStrings #-}

module Tools
  ( calculatorTool,
    weatherTool,
  )
where

import Agent
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T

-- | Calculator tool with proper expression parsing
calculatorTool :: Tool ()
calculatorTool =
  makeTool
    "calculator"
    "Evaluate arithmetic expressions. Supports +, -, *, /, parentheses, and proper operator precedence."
    ( simpleSchema
        [ ("expression", "string", "Math expression like '2 + 3', '10 * (5 + 2)', or '22 * 9/5 + 32'")
        ]
    )
    (\_ input -> pure $ calculate input)

calculate :: Value -> Either Text Value
calculate (Object obj) = case KM.lookup "expression" obj of
  Just (String expr) -> case parseExpr (T.unpack expr) of
    Just (result, rest) | all (== ' ') rest -> Right $ String $ T.pack $ show result
    Just (_, rest) -> Left $ "Unexpected input: " <> T.pack rest
    Nothing -> Left $ "Could not parse: " <> expr
  _ -> Left "Missing expression"
calculate _ = Left "Invalid input"

--------------------------------------------------------------------------------
-- Recursive descent parser for arithmetic expressions
-- Handles: +, -, *, /, parentheses, and proper operator precedence
--
-- Grammar:
--   expr   = term (('+' | '-') term)*
--   term   = factor (('*' | '/') factor)*
--   factor = number | '(' expr ')' | '-' factor
--------------------------------------------------------------------------------

type Parser a = String -> Maybe (a, String)

parseExpr :: Parser Double
parseExpr = chainl1 parseTerm parseAddOp

parseTerm :: Parser Double
parseTerm = chainl1 parseFactor parseMulOp

parseFactor :: Parser Double
parseFactor input = case skipSpaces input of
  '(' : rest -> do
    (val, rest') <- parseExpr rest
    case skipSpaces rest' of
      ')' : rest'' -> Just (val, rest'')
      _ -> Nothing
  '-' : rest -> do
    (val, rest') <- parseFactor rest
    Just (negate val, rest')
  _ -> parseNumber input

parseNumber :: Parser Double
parseNumber input =
  let s = skipSpaces input
      (numStr, rest) = span isNumChar s
   in if null numStr
        then Nothing
        else case reads numStr of
          [(n, "")] -> Just (n, rest)
          _ -> Nothing
  where
    isNumChar c = c `elem` ("0123456789." :: String)

parseAddOp :: Parser (Double -> Double -> Double)
parseAddOp input = case skipSpaces input of
  '+' : rest -> Just ((+), rest)
  '-' : rest -> Just ((-), rest)
  _ -> Nothing

parseMulOp :: Parser (Double -> Double -> Double)
parseMulOp input = case skipSpaces input of
  '*' : rest -> Just ((*), rest)
  '/' : rest -> Just (safeDiv, rest)
  _ -> Nothing
  where
    safeDiv _ 0 = 1 / 0 -- infinity for div by zero
    safeDiv a b = a / b

-- | Parse left-associative binary operators
chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op input = do
  (x, rest) <- p input
  go x rest
  where
    go acc rest = case op rest of
      Just (f, rest') -> case p rest' of
        Just (y, rest'') -> go (f acc y) rest''
        Nothing -> Nothing
      Nothing -> Just (acc, rest)

skipSpaces :: String -> String
skipSpaces = dropWhile (== ' ')

--------------------------------------------------------------------------------
-- Weather tool (mock)
--------------------------------------------------------------------------------

weatherTool :: Tool ()
weatherTool =
  makeTool
    "get_weather"
    "Get current weather for a city"
    ( simpleSchema
        [ ("city", "string", "City name")
        ]
    )
    (\_ input -> pure $ getWeather input)

getWeather :: Value -> Either Text Value
getWeather (Object obj) = case KM.lookup "city" obj of
  Just (String city) ->
    Right $
      String $
        "Weather in " <> city <> ": 22°C, partly cloudy"
  _ -> Left "Missing city"
getWeather _ = Left "Invalid input"
