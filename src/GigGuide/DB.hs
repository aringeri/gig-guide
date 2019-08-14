{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}
module GigGuide.DB
  ( module GigGuide.DB.Types
  , Event(..)
  , EventCategory(..)
  , Venue(..)
  , VenueLocation(..)
  , VenueCategory(..)
  , runPutMany
  , runStderrSqlite
  , selectAll
  , insertVenueCoords
  , migrateVenueGeos
  , selectEventsByVenue
  , EntityField(..)
  ) where

import           Database.Persist
import           Database.Persist.Sqlite
import           Control.Monad.IO.Unlift
import qualified Data.Text as T
import qualified Data.List.Split as S 
import           Control.Lens (_1, _2, _3, (%~), (&))
import           Control.Monad.Reader
import           Control.Monad.Logger
import           UnliftIO.Resource
import           Data.Time (Day)
import           GigGuide.Types.Geo (Latitude(..), Longitude(..), Coord(..), CoordBounds)
import           GigGuide.DB.Types
import           GigGuide.Util (grouping)
import           GigGuide.DB.Query

geoToLoc :: VenueGeo -> VenueLocation
geoToLoc g = VenueLocation
  (Latitude (venueGeoLat g))
  (Longitude (venueGeoLon g))

runStderrSqlite :: MonadUnliftIO m =>
                   T.Text 
                    -> ReaderT SqlBackend (LoggingT (ResourceT m)) a 
                    -> m a
runStderrSqlite c = runResourceT
                  . runStderrLoggingT 
                  . withSqliteConn c
                  . runSqlConn

runPutMany :: (PersistEntityBackend record ~ SqlBackend,
               PersistEntity record, 
               Control.Monad.IO.Unlift.MonadUnliftIO m) =>
               T.Text -> [record] -> m ()
runPutMany con e = runStderrSqlite con $ do
  runMigration migrateAll
  sequence_ $ putMany <$> S.chunksOf 30 e

selectAll :: (PersistEntityBackend record ~ SqlBackend,
              PersistEntity record, MonadUnliftIO m) =>
              T.Text -> m [Entity record]
selectAll c =
  runStderrSqlite c $ selectList [] []

groupByVenue :: [(VenueLocation, Venue, Event)] -> [((VenueLocation, Venue), [Event])]
groupByVenue = 
  grouping . fmap (\(l,v,e) -> ((l,v), e))

selectEventsByVenue :: MonadUnliftIO m =>
                        T.Text
                        -> Day 
                        -> CoordBounds 
                        -> m [((VenueLocation, Venue), [Event])]
selectEventsByVenue t d = 
    runStderrSqlite t . getGroupedEvents d

getGroupedEvents :: (MonadIO m,
                     BackendCompatible SqlBackend backend,
                     PersistQueryRead backend,
                     PersistUniqueRead backend) =>
                    Day
                    -> CoordBounds
                    -> ReaderT backend m [((VenueLocation, Venue), [Event])]
getGroupedEvents d b = 
  groupByVenue . fmap unwrapEntities 
    <$> selectBoundedEvents d b

unwrapEntities :: (Entity VenueGeo, Entity Venue, Entity Event)
                   -> (VenueLocation, Venue, Event)
unwrapEntities s =
  s & _1 %~ geoToLoc . entityVal
    & _2 %~ entityVal
    & _3 %~ entityVal

insertVenueCoords :: T.Text -> Coord -> Key Venue -> IO ()
insertVenueCoords conn c k =
  runStderrSqlite conn $
    putMany [VenueGeo k (getLat $ lat c) (getLon $ lon c)]
    

migrateVenueGeos :: MonadUnliftIO m => T.Text -> m ()
migrateVenueGeos c =
  runStderrSqlite c $ 
    runMigration (migrate entityDefs $ entityDef (Nothing :: Maybe VenueGeo))
