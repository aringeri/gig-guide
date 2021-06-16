{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleContexts           #-}
module Main where

import           Control.Monad (filterM)
import           Control.Monad.Reader
import           System.Environment (getArgs)
import           Data.Text (Text, pack, unpack)
import           Database.Persist
import           GigGuide.DB ( migrateVenueGeos, insertVenueCoords
                        , Venue(..), EntityField(..), selectAll
                        , runStderrSqlite)
import           GigGuide.Types.Geo (Coord)
import           GigGuide.Geocode
import           GigGuide.Geocode.Nominatim

type Geocoder a = ReaderT Config IO a

askConnStr :: (MonadReader Config m) => m Text
askConnStr = reader (unConnStr . dbConn)

main :: IO ()
main = fmap parseArgs getArgs
    >>= maybe printUsage runGeocoder

runGeocoder :: Config -> IO ()
runGeocoder = runReaderT geocoder

geocoder :: (MonadReader Config m, MonadIO m) => m ()
geocoder = do
  cstr <- askConnStr
  venues <- liftIO $ selectAll cstr
  if not (null venues) 
    then do
      liftIO $ migrateVenueGeos cstr
      sequence_ $ lookupAndInsert <$> venues
    else liftIO . putStrLn $ 
          "Warning : No Venues Found in db \"" 
                    ++ unpack cstr ++ "\""

parseArgs :: [String] -> Maybe Config
parseArgs [conn, api, url] = 
  Just $ Config (ConnStr . pack $ conn)
                (ApiKey . pack  $ api)
                url
parseArgs _      = Nothing

printUsage :: IO ()
printUsage = putStr "Unrecognized arguments\
  \\n  Usage : geocode-venues db_connection api_key geocode_url\
  \\n\
  \\neg connection : \"host=localhost port=5000\"\
  \\napi_key : geocoding API key\
  \\ngeocode_url : geocoding service url\n"

retryVenuesWithoutGeo :: (MonadReader Config m, MonadIO m) => m ()
retryVenuesWithoutGeo = do
  vs   <- selectVenuesWithoutGeo
  sequence_ (lookupAndInsert <$> vs)

selectVenuesWithoutGeo :: (MonadReader Config m, MonadIO m) =>
                          m [Entity Venue]
selectVenuesWithoutGeo = do
  cstr <- askConnStr
  vs   <- liftIO $ selectAll cstr
  liftIO $ filterM
     (\v -> (==0) <$> runStderrSqlite cstr (count [VenueGeoVenueId ==. entityKey v]))
     vs

lookupAndInsert :: (MonadReader Config m, MonadIO m) =>
                   Entity Venue -> m ()
lookupAndInsert e =
  logOrInsert =<< lookupCoords e

lookupCoords :: (MonadReader Config m, MonadIO m) =>
                Entity Venue -> m (Key Venue, Either String Coord)
lookupCoords e = do
  url <- asks geocoderEndpoint
  coords <- geocode url . entityVal $ e
  pure
    (entityKey e
    , maybe (Left $ "Geocoding failed for: " ++ show e)
            Right
            coords)

logOrInsert :: (MonadReader Config m, MonadIO m) =>
               (Key Venue, Either String Coord) -> m ()
logOrInsert coords = do
  conn <- askConnStr
  liftIO $ case coords of
    (v, Right c) -> insertVenueCoords conn c v
    (_, Left e)  -> putStrLn e
