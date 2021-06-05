{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
module GigGuide.Scraper.Beat.EventIntegrationSpec(spec) where

import GigGuide.Scraper.Beat.Internal
import GigGuide.Types.Event
import Test.Hspec
import Data.Maybe
import System.Environment

spec :: Spec
spec = do
  url <- runIO getUrlFromEnv
  let eventsUrl = url ++ "/gig-guide"
  describe "event scraper" $ do
    it "should scrape all events from the web app" $ do
      scrapeEvents eventsUrl defaultParams `shouldReturn` expectedEvents

getUrlFromEnv :: IO String 
getUrlFromEnv = fromMaybe "http://localhost:8080" <$> lookupEnv "BEAT_URL"  

expectedEvents :: [Either EventCreationError Event]
expectedEvents = []
