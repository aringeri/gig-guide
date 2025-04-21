{-# LANGUAGE OverloadedStrings #-}
module GigGuide.BeatSpec where

import Data.Time (fromGregorian)
import Data.Either()
import GigGuide.Scraper.BeatScraper
import Control.Monad.Logger
import Control.Monad.Identity
import GigGuide.Scraper.Beat.Internal(parseDay, EventOverview(..))
import Text.HTML.Scalpel
import Test.Hspec
import qualified Data.Text.Lazy as L
import GigGuide.Types.EventOverview (EventCategory(..))
import GigGuide.Types (mkPriceRange)

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
        parseEventOverviews f `shouldBe` (expectedEventOverviews, [])
        where 
          parseEventOverviews :: L.Text -> (EventOverviewScrapeResult, [LogLine])
          parseEventOverviews str = 
            runIdentity $ runWriterLoggingT (scrapeEventOverviews str)

          scrapeEventOverviews :: MonadLogger m => L.Text ->  m EventOverviewScrapeResult
          scrapeEventOverviews str = scrapeStringLikeT str eventOverviews 

          expectedEventOverviews :: Maybe [Either EventCreationError EventOverview]
          expectedEventOverviews = pure [
              pure (EventOverview
                  "Dangerous Goods &#038; Aura present Astrix &#038; Victor Ruiz"
                  "https://beat.com.au/dangerous-goods-aura-present-astrix-victor-ruiz/"
                  (fromGregorian 2025 04 18)
                  [Music]
                  ""
                  (Left 149.05)
                )
              , pure (EventOverview
                  "Main Stage Festival Showcase"
                  "https://beat.com.au/main-stage-festival-showcase-2/"
                  (fromGregorian 2025 04 18)
                  [Comedy]
                  "https://beat.com.au/directory/comedy-republic/"
                  (Right $ mkPriceRange 23.50 25.50)
                )
              , pure (EventOverview
                  "Two Hearts: Heartcore Karaoke"
                  "https://beat.com.au/two-hearts-heartcore-karaoke-2/"
                  (fromGregorian 2025 04 18)
                  [Music, Comedy]
                  "https://beat.com.au/directory/comedy-republic/"
                  (Right $ mkPriceRange 28.50 31.50)
                )
            ]
        