{-# LANGUAGE OverloadedStrings          #-}
module GigGuide.Geocode.GeocodeIntegrationSpec where

import Control.Monad.Reader
import Test.Hspec
import GigGuide.Geocode
import GigGuide.Geocode.Google
import GigGuide.Types.Geo (Coord(..), Latitude(..), Longitude(..))
import GigGuide.DB(Venue(..), VenueCategory (..))

config :: Config
config = Config 
  { dbConn = ConnStr "NOT USED"
  , apiKey = ApiKey "APIKEY"
  , geocoderEndpoint = "http://localhost:8081/maps/api/geocode/json" }

spec :: Spec
spec =
  describe "google geocoder" $ do
    it "should call service to geocode venue address" $ do
      runGeocode venue config `shouldReturn` 
        Just (Coord 
                (Latitude (-37.81194738558151)) 
                (Longitude 144.96788006894667))

venue :: Venue
venue = Venue 
      { venueName = "170 Russell"
      , venueAddress = "170 Russell St\n                            Melbourne VIC 3000"
      , venueUrl = "https://beat.com.au/directory/170-russell/"
      , venueCategories = [Club, LiveMusic]
      , venueCity = Just "Melbourne, VIC"
      }

runGeocode :: Venue -> Config -> IO (Maybe Coord)
runGeocode v = 
  runReaderT (geocode v)
  