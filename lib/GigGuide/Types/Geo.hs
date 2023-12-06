{-# LANGUAGE DeriveGeneric #-}
module GigGuide.Types.Geo
  ( Latitude(..)
  , Longitude(..)
  , Coord(..)
) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), toJSON)

newtype Latitude = Latitude { getLat :: Double }
  deriving (Eq, Show, Read, Ord, Generic)

newtype Longitude = Longitude { getLon :: Double}
  deriving (Eq, Show, Read, Ord, Generic)

instance FromJSON Latitude where
  parseJSON = fmap Latitude . parseJSON

instance ToJSON Latitude where
  toJSON = toJSON . getLat

instance FromJSON Longitude where
  parseJSON = fmap Longitude . parseJSON

instance ToJSON Longitude where
  toJSON = toJSON . getLon

data Coord = Coord
  { lat :: Latitude
  , lon :: Longitude
  } deriving (Eq, Ord, Show, Read, Generic)

instance FromJSON Coord
instance ToJSON Coord