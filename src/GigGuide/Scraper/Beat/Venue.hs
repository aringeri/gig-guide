{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
module GigGuide.Scraper.Beat.Venue 
 ( venues
 , VenueCreationError(..)
 ) where

import           Control.Monad (guard)
import           Control.Monad.Except
import           Control.Lens

import           Text.HTML.Scalpel.Core ( Scraper, (//), atDepth
                                        , hasClass, (@:), textSelector)
import           Text.HTML.Scalpel.Class

import           GigGuide.DB ( Venue(..), VenueCategory(..))
import           GigGuide.Scraper.Common

import qualified Data.Text.Lazy as L

data VenueCreationError = Blank
  deriving (Show, Eq)

venues :: Scraper L.Text [Either VenueCreationError Venue]
venues = do 
  vs <- chroot ("div" @: [hasClass "loaded-content"])
        $ chroots' ("article" @: [hasClass "article-card-row"]) venue
  guard (not (null vs)) 
  return vs

venue :: ( MonadError VenueCreationError m
         , MonadScraper L.Text m) =>
         m Venue
venue = do
  url <- L.unpack <$> attr "href" "a"
  chroot ("div" @: [hasClass "row"]) $
    Venue 
      <$> name
      <*> address
      <*> pure url
      <*> categories
      <*> city
  where 
    name    = cleanTexts ("div" // "div" // textSelector `atDepth` 1)
    address = cleanTexts ("div" // "div" @: [hasClass "venue-address"])
    categories = parseVenueCategories . L.strip <$> text ("div" // "div" @: [hasClass "article-categories"])
    city = selectCity <$> texts ("div" // "div" @: [hasClass "article-categories"])
    selectCity ts = 
      ts ^? element 1
        <&> L.strip
        >>= nonEmpty

nonEmpty :: L.Text -> Maybe L.Text
nonEmpty it | L.null it = Nothing
            | otherwise = Just it

parseVenueCategories :: L.Text -> [VenueCategory]
parseVenueCategories = parseCommaSep parseVenueCategory

parseVenueCategory :: L.Text -> VenueCategory
parseVenueCategory s = case s of 
  "Bars"       -> Bar
  "Clubs"      -> Club
  "Venues"     -> EventVenue
  "Live Music" -> LiveMusic
  "Pubs"       -> Pub
  "Breweries & Distilleries" -> Brewery
  "Festivals"  -> FestivalVenue
  "Theatres"   -> TheatreVenue
  o            -> OtherVenue o
