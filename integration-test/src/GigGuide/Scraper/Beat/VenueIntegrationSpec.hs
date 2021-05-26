{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
module GigGuide.Scraper.Beat.VenueIntegrationSpec(spec) where

import GigGuide.Scraper.Beat.Internal(scrapeVenues, venueSearchParams)
import GigGuide.DB.Internal
import Test.Hspec
import Data.Maybe
import System.Environment

spec :: Spec
spec = do
  url <- runIO getUrlFromEnv
  let directoryUrl = url ++ "/directory"
  describe "venue scraper" $ do
    it "should scrape all venues from the web app" $ do
      scrapeVenues directoryUrl venueSearchParams `shouldReturn` expectedVenues

getUrlFromEnv :: IO String 
getUrlFromEnv = fromMaybe "http://localhost:8080" <$> lookupEnv "BEAT_URL"  

expectedVenues :: [Venue]
expectedVenues =
  [ Venue {venueName = "170 Russell", venueAddress = "170 Russell St\n                            Melbourne VIC 3000", venueUrl = "https://beat.com.au/directory/170-russell/", venueCategories = [Club, LiveMusic], venueCity = Just "Melbourne, VIC"},
    Venue {venueName = "2 Brothers Brewery", venueAddress = "4 Joyner St\n                            Moorabbin VIC 3189", venueUrl = "https://beat.com.au/directory/2-brothers-brewery/", venueCategories = [LiveMusic, Brewery], venueCity = Just "Moorabbin, VIC"},
    Venue {venueName = "24 Moons", venueAddress = "2 Arthurton Rd\n                            Northcote VIC 3070", venueUrl = "https://beat.com.au/directory/24-moons/", venueCategories = [Bar, Club], venueCity = Just "Northcote, VIC"},
    Venue {venueName = "170 Russell", venueAddress = "170 Russell St\n                            Melbourne VIC 3000", venueUrl = "https://beat.com.au/directory/170-russell/", venueCategories = [Club, LiveMusic], venueCity = Just "Melbourne, VIC"},
    Venue {venueName = "2 Brothers Brewery",venueAddress = "4 Joyner St\n                            Moorabbin VIC 3189", venueUrl = "https://beat.com.au/directory/2-brothers-brewery/", venueCategories = [LiveMusic, Brewery], venueCity = Just "Moorabbin, VIC"},
    Venue {venueName = "24 Moons", venueAddress = "2 Arthurton Rd\n                            Northcote VIC 3070", venueUrl = "https://beat.com.au/directory/24-moons/", venueCategories = [Bar, Club], venueCity = Just "Northcote, VIC"},
    Venue {venueName = "Abbotsford Convent", venueAddress = "1 St Heliers St\n                            Abbotsford VIC 3067", venueUrl = "https://beat.com.au/directory/170-russell/", venueCategories = [OtherVenue ""], venueCity = Just "Abbotsford, VIC"},
    Venue {venueName = "Arts Centre Melbourne", venueAddress = "100 St Kilda Rd\n                            Melbourne VIC 3000", venueUrl = "https://beat.com.au/directory/2-brothers-brewery/", venueCategories = [LiveMusic, TheatreVenue], venueCity = Just "Melbourne, VIC"},
    Venue {venueName = "Bar 303", venueAddress = "303 High St\n                            Northcote VIC 3070", venueUrl = "https://beat.com.au/directory/24-moons/", venueCategories = [Bar, LiveMusic], venueCity = Just "Northcote, VIC"}
  ]