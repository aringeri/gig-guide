{-# LANGUAGE OverloadedStrings          #-}
module GigGuide.Geocode.GeocodeIntegrationSpec where

import Data.Maybe
import System.Environment
import Control.Monad.Reader
import Test.Hspec
import GigGuide.Geocode
import GigGuide.Geocode.Google
import GigGuide.Types.Geo (Coord(..), Latitude(..), Longitude(..))
import GigGuide.DB(Venue(..), VenueCategory (..))

baseConfig :: Url -> Config
baseConfig = Config 
  (ConnStr "NOT USED")
  (ApiKey "APIKEY")

getConfigFromEnv :: IO Config 
getConfigFromEnv = baseConfig 
  <$> (fromMaybe "http://localhost:8081/maps/api/geocode/json" <$> lookupEnv "GEOCODE_URL")

spec :: Spec
spec = do
  config <- runIO getConfigFromEnv
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
  