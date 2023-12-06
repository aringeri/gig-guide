{-# LANGUAGE DeriveGeneric #-}
module GigGuide.Types.VenueAndGeo
( VenueAndGeo(..)
) where

import GigGuide.Types.Venue (Venue)
import GigGuide.Types.Geo (Coord)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

data VenueAndGeo = VenueAndGeo
  { venue :: Venue
  , geo :: Coord
  } deriving (Show, Eq, Ord, Generic)

instance ToJSON VenueAndGeo
instance FromJSON VenueAndGeo