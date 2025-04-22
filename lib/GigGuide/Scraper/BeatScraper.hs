{-# LANGUAGE OverloadedStrings #-}
module GigGuide.Scraper.BeatScraper
  (
  mkOpts,
  eventSearchDayParam,
  eventOverviews,
  eventDetail,
  scrapeVenues,
  iteratePages,
  EventSearchParams(..),
  EventCreationError(..)
  )
  where

import GigGuide.Scraper.Beat.EventOverview
import GigGuide.Scraper.Beat.EventDetails
import GigGuide.Scraper.Beat.Venue
import Data.Time (Day, fromGregorian)
import Control.Lens
import GigGuide.Scraper.Common (incMaybe, optionL, fmtDate)
import Network.Wreq ( Options, defaults, params)

data EventSearchParams =
  EventSearchParams
    {
      -- keywordParam :: T.Text,
      dateParam :: Day
    -- , radiusParam :: Int
    , eventReload :: Maybe Integer
    -- , cityLat :: Maybe Double
    -- , cityLon :: Maybe Double 
    } deriving (Eq, Show)

mkOpts :: EventSearchParams -> Options
mkOpts s = defaults & params .~ mkParams s
  where mkParams (EventSearchParams date reload) =
          ("start_date", fmtDate date)
          : ("end_date", fmtDate date)
          : optionL "reload" reload

eventReloadL :: Lens' EventSearchParams (Maybe Integer)
eventReloadL = lens eventReload (\p r -> p { eventReload = r })

iteratePages :: EventSearchParams -> [EventSearchParams]
iteratePages = iterate incrementPage
  where
    incrementPage :: EventSearchParams -> EventSearchParams
    incrementPage = eventReloadL %~ incMaybe

defaultEventSearchParams :: EventSearchParams
defaultEventSearchParams = EventSearchParams
  (fromGregorian 2019 8 1)
  (Just 1)

eventSearchDayParam :: Day -> EventSearchParams
eventSearchDayParam d = defaultEventSearchParams { dateParam = d }
