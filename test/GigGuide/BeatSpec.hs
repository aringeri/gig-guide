{-# LANGUAGE OverloadedStrings #-}
module GigGuide.BeatSpec where

import Data.Time (fromGregorian)
import GigGuide.Scraper.BeatScraper
import Control.Monad.Logger
import Control.Monad.Identity
import GigGuide.Scraper.Beat.Internal(parseDay, EventOverview(..))
import Text.HTML.Scalpel
import Test.Hspec
import qualified Data.Text.Lazy as L

type EventOverviewScrapeResult = Maybe [Either EventCreationError EventOverview]

spec :: Spec
spec = do
  describe "date parser" $ do
    it "should return error on empty string" $ do
      parseDay "" `shouldBe` Left (DateParseError "")
    it "should include date string on error" $ do
      parseDay "not-a-day" `shouldBe` Left (DateParseError "not-a-day")
    it "should parse a simple date" $ do
      parseDay "Sat 10 Feb 2024" `shouldBe` Right (fromGregorian 2024 2 10)
    it "should take first day from date range" $ do
      parseDay "Fri 01 - Sun 03 Dec, 2023" `shouldBe` Right (fromGregorian 2023 12 1)
  before (L.pack <$> readFile "data/responses/simple-event-overview.html") $ do
    describe "event overview scraper" $ do
      it "should parse event overviews" $ \f -> do
        parseEventOverviews f `shouldBe` (Nothing, [])
        where 
          parseEventOverviews :: L.Text -> (EventOverviewScrapeResult, [LogLine])
          parseEventOverviews str = 
            runIdentity $ runWriterLoggingT (scrapeEventOverviews str)

          scrapeEventOverviews :: MonadLogger m => L.Text ->  m EventOverviewScrapeResult
          scrapeEventOverviews str = scrapeStringLikeT str eventOverviews 
        