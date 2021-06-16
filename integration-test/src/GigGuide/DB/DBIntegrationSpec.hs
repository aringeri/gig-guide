{-# LANGUAGE OverloadedStrings #-}
module GigGuide.DB.DBIntegrationSpec where

import GigGuide.DB.Internal
import GigGuide.DB
import GigGuide.Types.Geo
import Database.Persist
import Test.Hspec
import System.Directory ( removeFile )
import Control.Exception ( catch, throwIO )
import System.IO.Error ( isDoesNotExistError )
import Data.Text ( Text, unpack )

dbFile :: Text
dbFile = "integration-test.db"

dbFilePath :: FilePath 
dbFilePath = unpack dbFile

spec :: Spec
spec = do
  afterAll_ (rmDbFile dbFilePath) $ do
    before (rmDbFile dbFilePath) $ do
      describe "when running 'putMany'" $ do
        it "should store multiple venues in the db" $ do
          runPutMany dbFile venues
          actualVs <- fetchVenues dbFile
          actualVs `shouldMatchList` venues
      
      before_ (migrateVenueGeos dbFile >> migrateVenues dbFile) $ do
        describe "insertVenueCoords" $ do
          context "given a venue is in the database" $ do
            before (insertVenue dbFile) $ do
              it "should store coordinates for the given venue" $ \venue -> do
                insertVenueCoords dbFile testCoord venue
                venueGeos <- selectVenueGeos dbFile
                venueGeos `shouldContainSingleCoord` testCoord
              it "should use the inserted venue as the key" $ \venue -> do
                insertVenueCoords dbFile testCoord venue
                venueGeos <- selectVenueGeos dbFile
                (venueGeoVenueId . entityVal . head) venueGeos `shouldBe` venue
      
migrateVenues :: Text -> IO ()
migrateVenues db = migrateEntity db (Nothing :: Maybe Venue)

shouldContainSingleCoord :: [Entity VenueGeo] -> Coord -> Expectation
shouldContainSingleCoord vs c = vs `shouldSatisfy` containsOnly c

containsOnly :: Coord -> [Entity VenueGeo] -> Bool
containsOnly coord geos =
  length geos == 1
    && venueGeoLat geo == getLat (lat coord)
    && venueGeoLon geo == getLon (lon coord)
  where geo = entityVal $ head geos

insertVenue :: Text -> IO (Key Venue)
insertVenue c = runStderrSqlite c $ insert russellStreet

selectVenueGeos :: Text -> IO [Entity VenueGeo]
selectVenueGeos c = runStderrSqlite c $ selectList [] []

testCoord :: Coord
testCoord = Coord (Latitude 1) (Longitude 4)

rmDbFile :: FilePath   -> IO ()
rmDbFile db = do
  putStrLn $ "removing DB file: " ++ db
  removeFile db `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e

venues :: [Venue]
venues = 
  [ russellStreet
    , Venue 
      { venueName = "2 Brothers Brewery"
      , venueAddress = "4 Joyner St\n                            Moorabbin VIC 3189"
      , venueUrl = "https://beat.com.au/directory/2-brothers-brewery/"
      , venueCategories = [LiveMusic, Brewery]
      , venueCity = Just "Moorabbin, VIC"
      }
  ]

russellStreet :: Venue
russellStreet = 
  Venue 
    { venueName = "170 Russell"
    , venueAddress = "170 Russell St\n                            Melbourne VIC 3000"
    , venueUrl = "https://beat.com.au/directory/170-russell-street/"
    , venueCategories = [Club, LiveMusic]
    , venueCity = Just "Melbourne, VIC"
    }

fetchVenues :: Text -> IO [Venue]
fetchVenues db = do
  vs <- runStderrSqlite db $ selectList [] []
  return $ entityVal <$> vs
