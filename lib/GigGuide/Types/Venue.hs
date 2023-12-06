{-# LANGUAGE DeriveGeneric #-}
module GigGuide.Types.Venue 
  ( VenueCategory(..)
  , Venue(..)
) where

import Data.Text.Lazy(Text)
import GigGuide.Types (URL)
import GHC.Generics
import Data.Aeson (FromJSON, ToJSON)

data VenueCategory =
    Bar
  | Club
  | EventVenue
  | LiveMusic
  | Pub
  | Brewery
  | FestivalVenue
  | TheatreVenue
  | OtherVenue Text
  deriving (Eq, Show, Read, Ord, Generic)

instance FromJSON VenueCategory
instance ToJSON VenueCategory

data Venue = Venue
  { name :: Text
  , address :: Text
  , url :: URL
  , categories :: [VenueCategory]
  , city :: Maybe Text
} deriving (Show, Ord, Eq, Generic)

instance FromJSON Venue
instance ToJSON Venue