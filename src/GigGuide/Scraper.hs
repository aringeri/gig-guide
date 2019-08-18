{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
module GigGuide.Scraper
  ( scrapeEvents
  , dayParams
  , fromDB
  , toDB
  , scrapeVenues
  , venueSearchParams) where

import           Data.Maybe (fromMaybe)
import           Data.Either (partitionEithers)
import           Control.Monad.Except
import           Control.Lens
import           Data.Time (Day, fromGregorian, defaultTimeLocale)
import           Data.Time.Format (formatTime)
import           Text.HTML.Scalpel.Core ( Scraper, scrapeStringLike)
import           Network.Wreq ( Options, getWith, defaults
                              , params, responseBody)

import           GigGuide.DB (Venue(..))
import           GigGuide.Types (URL)
import           GigGuide.Types.Event (Event(..))
import           GigGuide.Scraper.Beat

import qualified Data.ByteString.Lazy as B
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Encoding as LE

data SearchParams = 
  EventParams
    { keywordParam :: T.Text
    , dateParam :: Day
    , radiusParam :: Int
    , reloadParam :: Maybe Integer
    , cityLat :: Maybe Double
    , cityLon :: Maybe Double 
    }
  | VenueParams 
    { reloadParam :: Maybe Integer 
    } deriving (Eq, Show)

defaultParams :: SearchParams
defaultParams = EventParams 
  ""
  (fromGregorian 2019 8 1) 
  15
  Nothing
  Nothing
  Nothing

dayParams :: Day -> SearchParams
dayParams d = defaultParams { dateParam = d }

venueSearchParams :: SearchParams
venueSearchParams = VenueParams Nothing

fetchPage :: URL -> SearchParams -> IO B.ByteString
fetchPage url s = do
  let opts = mkOpts s
  r <- getWith opts url
  pure $ r ^. responseBody

mkOpts :: SearchParams -> Options
mkOpts s = defaults & params .~ mkParams s
  where mkParams (EventParams keyword date radius reload _ _) =
          [ ("keyword", keyword)
          , ("date", fmtDate date)
          , ("radius", showt radius)]
          ++ option "reload" reload
        mkParams (VenueParams reload) = option "more" reload
        option k = maybe [] ( (:[]) . (,) k . T.pack . show)

showt :: Show a => a -> T.Text
showt = T.pack . show

fmtDate :: Day -> T.Text
fmtDate = T.pack . formatTime defaultTimeLocale "%d.%m.%_Y"

scrapeEvents :: URL -> SearchParams -> IO [Event]
scrapeEvents u s = printLefts (crawlEvents u s)

scrapeVenues :: URL -> SearchParams -> IO [Venue]
scrapeVenues u s = printLefts (runScraper u s venues)

printLefts :: Show e => IO [Either e a] -> IO [a]
printLefts a = do
  (errs,as) <- partitionEithers <$> a
  forM_ errs print
  return as

crawlEvents :: URL -> SearchParams -> IO [Either EventCreationError Event]
crawlEvents u s = do
  rs <- runScraper u s events
  forM rs (fmap join . mapM scrape)
  where 
    mapNone = fromMaybe (Left ExtraDetailsMissingError)
    scrape = fmap mapNone . scrapeEventDetails

runScraper :: URL -> SearchParams -> Scraper L.Text [a] -> IO [a]
runScraper u s = 
  fmap concat . scrapeAll u s

maxFetches :: Int
maxFetches = 250

scrapeAll :: URL -> SearchParams -> Scraper L.Text a -> IO [a]
scrapeAll url p scr =
  run (take maxFetches $ iterate incReload p)
  where incReload s = s { reloadParam = Just $ maybe 1 (+1) (reloadParam s) } 
        run []     = return []
        run (a:as) = do
          c <- fetchPage url a
          case scrapeStringLike (LE.decodeUtf8 c) scr of
            Just e ->
              (e :) <$> run as
            _      -> return []

