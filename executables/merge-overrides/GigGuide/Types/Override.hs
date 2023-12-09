{-# LANGUAGE DeriveGeneric #-}
module GigGuide.Types.Override
( VenueOverride(..)
, VenuePropertyOverrides(..)
) where
import GigGuide.Types (URL)
import GigGuide.Types.Geo (Coord)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

data VenueOverride = VenueOverride
    { url :: URL
    , venueOverride :: VenuePropertyOverrides
    } deriving (Eq, Show, Generic)

instance ToJSON VenueOverride
instance FromJSON VenueOverride


data VenuePropertyOverrides = VenuePropertyOverrides
    { geo :: Coord
    } deriving (Eq, Show, Generic)

instance ToJSON VenuePropertyOverrides
instance FromJSON VenuePropertyOverrides