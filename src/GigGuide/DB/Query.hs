{-# LANGUAGE FlexibleContexts           #-}
module GigGuide.DB.Query
  ( selectBoundedEvents
  , venueInBounds
  ) where

import           Data.Time (Day)
import           Control.Monad.Reader (ReaderT, MonadIO)
import           Database.Esqueleto
import           GigGuide.DB.Types (VenueGeo(..), Venue(..),
                               Event(..), EntityField(..))
import           GigGuide.Types.Geo (Coord(..), Latitude(..), Longitude(..),
                               CoordBounds, southWest, northEast)

selectBoundedEvents :: (PersistUniqueRead backend,
                        PersistQueryRead backend, 
                        BackendCompatible SqlBackend backend,
                        MonadIO m) =>
                       Day
                        -> CoordBounds
                        -> ReaderT
                           backend m [(Entity VenueGeo, Entity Venue, Entity Event)]
selectBoundedEvents d b = 
  select $
  from $ \(g `InnerJoin` v `InnerJoin` e) -> do
  on (e ^. EventLocation ==. v ^. VenueName)
  on (v ^. VenueId ==. g ^. VenueGeoVenueId)
  where_ (venueInBounds g b
      &&. e ^. EventDate ==. val d)
  return (g, v, e)

venueInBounds :: SqlExpr (Entity VenueGeo)
                  -> CoordBounds-> SqlExpr (Value Bool)
venueInBounds g b =
  let s = latVal . southWest $ b
      w = lonVal . southWest $ b
      n = latVal . northEast $ b
      e = lonVal . northEast $ b
  in    g ^. VenueGeoLat <=. val n
    &&. g ^. VenueGeoLat >=. val s
    &&. g ^. VenueGeoLon >=. val w
    &&. g ^. VenueGeoLon <=. val e

latVal :: Coord -> Double
latVal = getLat . lat

lonVal :: Coord -> Double
lonVal = getLon . lon
