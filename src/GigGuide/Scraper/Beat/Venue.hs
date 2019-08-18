{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
module GigGuide.Scraper.Beat.Venue 
 ( VenueUrlParams(..)
 , VenueCreationError(..)
 , venueSearchParams
 , scrapeVenues
 ) where

import           Control.Monad.Except
import           Control.Lens
import qualified Data.List.NonEmpty as N

import           Text.HTML.Scalpel.Core ( Scraper, (//), atDepth
                                        , hasClass, (@:), textSelector)
import           Text.HTML.Scalpel.Class
import           Network.Wreq (Options, defaults, param)

import           GigGuide.Types (URL)
import           GigGuide.DB (Venue(..), VenueCategory(..))
import           GigGuide.Scraper.Common

import qualified Data.Text as T
import qualified Data.Text.Lazy as L

newtype VenueUrlParams =  
  VenueUrlParams 
    { venueReload :: Maybe Integer 
    } deriving (Eq, Show)

venueReloadL :: Lens' VenueUrlParams (Maybe Integer)
venueReloadL = lens venueReload (\p r -> p { venueReload = r })

mkOpts :: VenueUrlParams -> Options
mkOpts (VenueUrlParams (Just r)) = 
  defaults & param "more" .~ [(T.pack . show) r]
mkOpts _ = defaults

venueSearchParams :: VenueUrlParams
venueSearchParams = VenueUrlParams Nothing

itPages :: VenueUrlParams -> [VenueUrlParams]
itPages = iterate incPage

incPage :: VenueUrlParams -> VenueUrlParams
incPage = venueReloadL %~ incMaybe

scrapeVenues :: URL -> VenueUrlParams -> IO [Venue]
scrapeVenues u s = printLefts (runScraper (zipUrls u s) venues)

zipUrls :: URL -> VenueUrlParams -> [(URL, Options)]
zipUrls u p = 
  zip (replicate maxFetches u)
      (mkOpts <$> itPages p)

data VenueCreationError = Blank
  deriving (Show, Eq)

venues :: Scraper L.Text (N.NonEmpty (Either VenueCreationError Venue))
venues = do 
  vs <- chroot ("div" @: [hasClass "loaded-content"])
        $ chroots' ("article" @: [hasClass "article-card-row"]) venue
  failEmpty vs

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
