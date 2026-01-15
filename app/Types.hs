{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Domain types for the weather pipeline example
--
-- ToSchema instances are derived automatically via GHC Generics!
module Types
  ( -- * Weather Data Types
    WeatherInfo (..)
  , ConvertedTemperature (..)
  , WeatherReport (..)
  ) where

import Agent (ToSchema)
import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

-- | Weather information from the weather service
data WeatherInfo = WeatherInfo
  { wiCity :: Text
  , wiTemperatureC :: Double
  , wiCondition :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON WeatherInfo where
  parseJSON = withObject "WeatherInfo" $ \o ->
    WeatherInfo
      <$> o .: "city"
      <*> o .: "temperature_c"
      <*> o .: "condition"

instance ToJSON WeatherInfo
instance ToSchema WeatherInfo  -- Derived automatically!

-- | Temperature in multiple units
data ConvertedTemperature = ConvertedTemperature
  { ctCelsius :: Double
  , ctFahrenheit :: Double
  , ctKelvin :: Double
  }
  deriving (Show, Eq, Generic)

instance FromJSON ConvertedTemperature where
  parseJSON = withObject "ConvertedTemperature" $ \o ->
    ConvertedTemperature
      <$> o .: "celsius"
      <*> o .: "fahrenheit"
      <*> o .: "kelvin"

instance ToJSON ConvertedTemperature
instance ToSchema ConvertedTemperature  -- Derived automatically!

-- | Complete weather report with all data
data WeatherReport = WeatherReport
  { wrCity :: Text
  , wrCondition :: Text
  , wrTemperature :: ConvertedTemperature  -- Nested type works too!
  , wrFeelsLike :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON WeatherReport where
  parseJSON = withObject "WeatherReport" $ \o ->
    WeatherReport
      <$> o .: "city"
      <*> o .: "condition"
      <*> o .: "temperature"
      <*> o .: "feels_like"

instance ToJSON WeatherReport
instance ToSchema WeatherReport  -- Derived automatically!
