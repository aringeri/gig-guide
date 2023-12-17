{-# LANGUAGE OverloadedStrings #-}
module GigGuide.BeatSpec where

import Data.Time (fromGregorian)
import GigGuide.Scraper.Beat.Internal(parseDay, EventCreationError(..))
import Test.Hspec

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

      