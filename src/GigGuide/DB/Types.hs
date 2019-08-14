{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module GigGuide.DB.Types where

import           Data.Text.Lazy
import           Data.Time (Day)

import           Database.Persist.TH

import           GigGuide.Types.Geo (Latitude(..), Longitude(..))
import           GigGuide.Types (Price, URL)

data EventCategory =
    Free
  | Music
  | Festival
  | Comedy
  | Theatre
  | Dance
  | Trivia
  | Markets
  | Art
  | Other Text
  deriving (Eq, Show, Read, Ord)
derivePersistField "EventCategory"

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
  deriving (Eq, Show, Read, Ord)
derivePersistField "VenueCategory"

data VenueLocation = VenueLocation
  { venueLatitude  :: Latitude
  , venueLongitude :: Longitude
  } deriving (Eq, Show, Ord)

share [mkPersist sqlSettings, mkMigrate "migrateAll", mkSave "entityDefs"] [persistLowerCase|
Event
  name Text
  url URL
  UniqueEventUrl url
  date Day
  categories [EventCategory]
  location Text
  price Price
  upperPrice Price Maybe
  --eventVenue VenueId Maybe
  deriving Show Ord Eq
Venue
  name Text
  address Text
  url URL
  UniqueVenueUrl url
  categories [VenueCategory]
  city Text Maybe
  deriving Show Ord Eq
--VenueLocation
--  venueId VenueId
--  UniqueId venueId
--  lat Latitude
--  lon Longitude
VenueGeo
  venueId VenueId
  GeoUniqueId venueId
  lat Double
  lon Double
  deriving Show Ord Eq
|]