{-# LANGUAGE OverloadedStrings          #-}
module GigGuide.Geocode.GeocodeIntegrationSpec where

import Data.Maybe
import System.Environment
import Control.Monad.Reader
import Test.Hspec
import GigGuide.Geocode
import qualified GigGuide.Geocode.Google as G
import qualified GigGuide.Geocode.Nominatim as N
import GigGuide.Types.Geo (Coord(..), Latitude(..), Longitude(..))
import GigGuide.DB(Venue(..), VenueCategory (..))

baseConfig :: Url -> Config
baseConfig = Config 
  (ConnStr "NOT USED")
  (ApiKey "APIKEY")

spec :: Spec
spec = do
  googleUrl <- runIO $ fromJust <$> lookupEnv "GOOGLE_GEOCODE_URL"
  nominatimUrl <- runIO $ fromJust <$> lookupEnv "NOMINATIM_GEOCODE_URL"
  
  describe "google geocoder" $ do
    let config = baseConfig googleUrl
    it "should call service to geocode venue address" $ do
      runGoogleGeocode venue config `shouldReturn` 
        Just (Coord 
                (Latitude (-37.81194738558151)) 
                (Longitude 144.96788006894667))
  
  describe "nominatim geocoder" $ do
    let config = baseConfig nominatimUrl
    it "should call service to geocode venue address" $ do
      runNominatimGeocode venue config `shouldReturn` 
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

runGoogleGeocode :: Venue -> Config -> IO (Maybe Coord)
runGoogleGeocode v = 
  runReaderT (G.geocode v)

runNominatimGeocode :: Venue -> Config -> IO (Maybe Coord)
runNominatimGeocode v c =
  runReaderT (N.geocode (geocoderEndpoint c) v) c
  