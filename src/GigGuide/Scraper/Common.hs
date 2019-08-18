{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
module GigGuide.Scraper.Common where

import           Data.Char (isSpace)
import           Control.Monad.Fail (MonadFail)
import           Data.Time.Format (defaultTimeLocale, parseTimeM, ParseTime)
import           Text.HTML.Scalpel.Core (Selector)
import           Text.HTML.Scalpel.Class (MonadScraper, texts)

import qualified Data.Text.Lazy as L

parseTimeDefault :: (MonadFail m, ParseTime t) => 
                    String -> String -> m t
parseTimeDefault = parseTimeM True defaultTimeLocale

parseCommaSep :: (L.Text -> a) -> L.Text -> [a]
parseCommaSep f t = f . L.strip <$> L.splitOn "," t

cleanTexts :: (MonadScraper L.Text m) => Selector -> m L.Text
cleanTexts = fmap (L.strip . joinEmpty) . texts

joinEmpty :: [L.Text] -> L.Text
joinEmpty = L.concat . filter (L.any $ not . isSpace)