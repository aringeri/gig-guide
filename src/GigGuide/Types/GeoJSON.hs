{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module GigGuide.Types.GeoJSON
  ( FeatureCollection(..)
  , Feature(..)
  , Properties(..)
  , EventJSON(..)
  , Geometry(..)
  , ToJSON(..)
  ) where

import Data.Text.Lazy
import Data.Aeson (ToJSON(..), object, (.=))
import GHC.Generics
import GigGuide.Types (URL)

newtype FeatureCollection = FeatureCollection
  { features :: [Feature]
  } deriving (Eq, Show, Generic)

instance ToJSON FeatureCollection

data Feature = Feature
  { featureType :: String
  , geometry :: Geometry
  , properties :: Properties
  } deriving (Eq, Show, Generic)

data Properties = Properties
  { vName :: Text
  , events :: [EventJSON]
  } deriving (Eq, Show, Generic)
instance ToJSON Properties

data EventJSON = EventJSON
  { eName :: Text
  , eDate :: Text
  , price :: Text
  , categories :: [Text]
  , time :: String
  , genre :: Maybe Text
  , supports :: [Text]
  , ticketUrl :: Maybe URL
  } deriving (Eq, Show, Generic)
instance ToJSON EventJSON

instance ToJSON Feature where  
  toJSON (Feature t g p) = 
    object ["type" .= t, "geometry" .= g, "properties" .= p]

data Geometry = Geometry
  { geoType :: String
  , coordinates :: [Double]
  } deriving (Eq, Show, Generic)

instance ToJSON Geometry where
  toJSON (Geometry t c) = object ["type" .= t, "coordinates" .= c]
