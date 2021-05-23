{-# LANGUAGE OverloadedStrings #-}
module GigGuide.Scraper.Beat.VenueSpec(spec) where

import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L
import GigGuide.Scraper.Beat.Internal(venues, venue, VenueCreationError(..))
import GigGuide.DB.Internal
import Text.HTML.Scalpel.Core (scrapeStringLike)
import Text.HTML.Scalpel.Class(runScrapeM)
import Test.Hspec
import qualified Data.List.NonEmpty as NE

type ScrapeResult = NE.NonEmpty (Either VenueCreationError Venue)

spec :: Spec
spec = do
  describe "venues scraper" $ do
    it "should parse venue types from valid HTML" $ do
      scrapeVenuesFromVenueDirectoryFile `shouldReturn` expectedValidVenues 
    it "should provide an empty result when no more venues are found" $ do
      scrapeVenuesFromEmptyDirectoryFile `shouldReturn` Nothing
  describe "single venue scraper" $ do
    it "should return an error when venue name is missing" $ do
      scrapeVenue `shouldReturn` Just (Left $ MissingField "venueName")

scrapeVenuesFromVenueDirectoryFile :: IO (Maybe ScrapeResult)
scrapeVenuesFromVenueDirectoryFile = 
  scrapeVenues <$> venueDirectoryFile

scrapeVenuesFromEmptyDirectoryFile :: IO (Maybe ScrapeResult)
scrapeVenuesFromEmptyDirectoryFile = 
  scrapeVenues <$> L.readFile "test/resources/venues/directory-no-more-venues.html"

scrapeVenues :: L.Text -> Maybe ScrapeResult
scrapeVenues s = scrapeStringLike s venues

scrapeVenue :: IO (Maybe (Either VenueCreationError Venue))
scrapeVenue = do
  v <- L.readFile "test/resources/venues/fragments/missing-name.html"
  return $ runScrapeM venue v

venueDirectoryFile :: IO L.Text
venueDirectoryFile = L.readFile "test/resources/venues/directory.html"

expectedValidVenues :: Maybe ScrapeResult
expectedValidVenues = Just expectedVenues

expectedVenues :: ScrapeResult
expectedVenues = 
  Right <$> NE.fromList [ 
      Venue {
          venueName = "170 Russell"
        , venueAddress = "170 Russell St\n                            Melbourne VIC 3000"
        , venueUrl = "https://beat.com.au/directory/170-russell/"
        , venueCategories = [Club, LiveMusic]
        , venueCity = Just "Melbourne, VIC"
      }
    , Venue {
          venueName = "2 Brothers Brewery"
        , venueAddress = "4 Joyner St\n                            Moorabbin VIC 3189"
        , venueUrl = "https://beat.com.au/directory/2-brothers-brewery/"
        , venueCategories = [LiveMusic, Brewery]
        , venueCity = Just "Moorabbin, VIC"
      }
    , Venue {
          venueName = "24 Moons"
        , venueAddress = "2 Arthurton Rd\n                            Northcote VIC 3070"
        , venueUrl = "https://beat.com.au/directory/24-moons/"
        , venueCategories = [Bar, Club]
        , venueCity = Just "Northcote, VIC"
      }
  ]
