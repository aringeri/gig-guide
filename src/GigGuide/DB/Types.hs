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
module GigGuide.DB.Types
  ( module GigGuide.DB.EventGenre
  , EventCategory(..)
  , VenueCategory(..)
  , Event(..)
  , EventId
  , Venue(..)
  , VenueId
  , VenueGeo(..)
  , VenueGeoId
  , VenueLocation(..)
  , EntityField(..)
  , entityDefs
  , migrateAll) where

import           Data.Text.Lazy
import           Data.Time (Day, TimeOfDay)

import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.TH

import           GigGuide.DB.EventGenre
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
  date Day
  categories [EventCategory]
  location Text
  price Price
  upperPrice Price Maybe
  time TimeOfDay
  genre EventGenre Maybe
  supports [Text]
  venueUrl URL
  ticketUrl URL Maybe
  UniqueEvent name date venueUrl time
  deriving Show Ord Eq
Venue
  name Text
  address Text
  url URL
  UniqueVenueUrl url
  categories [VenueCategory]
  city Text Maybe
  deriving Show Ord Eq
VenueGeo
  venueId VenueId
  GeoUniqueId venueId
  lat Double
  lon Double
  deriving Show Ord Eq
|]