{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module GigGuide.Scraper.Beat.EventOverview
  ( EventOverview(..)
  , EventCreationError(..)
  , eventOverviews
  , parseDay
  ) where

import           Data.Time (Day)
import           Text.HTML.Scalpel

import           GigGuide.Types (mkPriceRange', Price, PriceRange, PriceRange(..))

import qualified Data.Text.Lazy as L
import GigGuide.Scraper.Common (joinEmpty, parseTimeDefault, parseCommaSep)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import GigGuide.Types.EventOverview (EventOverview(..), EventCategory(..))
import Data.Bifunctor (first)
import Control.Monad.Logger (MonadLogger)
import Control.Applicative ((<|>))

data EventCreationError = 
    DateParseError String
  | PriceParseError
  | PriceRangeParseError
  | ExtraDetailsMissingError
  | TimeParseError
  | VenueUrlMissingError
  | ErrorContext{ context :: String, err :: EventCreationError }
  deriving (Show, Eq)

eventOverviews :: MonadLogger m => ScraperT L.Text m [Either EventCreationError EventOverview]
eventOverviews = do
  es <- chroots (tagSelector "article") eventOverview
  if null es then fail "stopping on empty events" else pure es

eventOverview :: MonadLogger m => ScraperT L.Text m (Either EventCreationError EventOverview)
eventOverview = do
  url <- L.unpack <$> attr "href" "a"
  let row = "div" @: [hasClass "row"]
  chroot row $ do
    date <- parseDay . L.strip <$> 
              text ("div" @: [hasClass "article-categories"] // textSelector `atDepth` 1)
    cats <- parseEventCategories . L.strip <$> text ("div" @: [hasClass "visible-xs"])
    name <- cleanTexts $ "div" // "div" // textSelector `atDepth` 1
    location <- cleanTexts $ "div" 
                        // "div" @: [hasClass "article-card-meta-gig--details"] 
                        // textSelector `atDepth` 1
    price <- parsePriceRange <$> text ("span" @: [hasClass "price"])
    let overview = EventOverview name url
              <$> date 
              <*> pure cats 
              <*> pure location 
              <*> price
    let addErrorContext  = first (ErrorContext ("Parsing event overview, url=" ++ url))
    return $ addErrorContext overview
      

cleanTexts :: Monad m => Selector -> ScraperT L.Text m L.Text
cleanTexts = fmap (L.strip . joinEmpty) . texts

parseDay :: L.Text -> Either EventCreationError Day
parseDay t = maybe 
  (Left (DateParseError s))
  Right 
  (parseTimeDefault "%a %d %b %_Y" s <|> parseFirstDayInRange t)
  where s = L.unpack t

parseFirstDayInRange :: L.Text -> Maybe Day
parseFirstDayInRange s = 
  let 
    (dm, year) = L.breakOn "," s
    (day, dm') =  L.breakOn "-" dm
    month = mconcat $ drop 3 (L.words dm')
  in parseTimeDefault "%a %d %b %_Y" (L.unpack $ day <> month <> L.drop 1 year)

parsePriceRange :: L.Text -> Either EventCreationError (Either Price PriceRange)
parsePriceRange t = case L.splitOn "-" t of
  [p]    -> Left <$> parsePrice p
  [l, u] -> do 
    lp <- parsePrice l
    up <- parsePrice u
    maybe (Left PriceRangeParseError)
          (Right . Right)
          (mkPriceRange' lp up)
  _      -> Left PriceRangeParseError

parsePrice :: L.Text -> Either EventCreationError Price
parsePrice "FREE" = Right 0
parsePrice p = maybe (Left PriceParseError) Right 
  ((readMaybe . L.unpack)
  (fromMaybe p (L.stripPrefix "$" (L.stripStart p))))

parseEventCategories :: L.Text -> [EventCategory]
parseEventCategories = parseCommaSep parseEventCategory

parseEventCategory :: L.Text -> EventCategory
parseEventCategory c = case c of 
  "Free Events"       -> Free
  "Music"             -> Music
  "Festivals"         -> Festival
  "Comedy"            -> Comedy
  "Theatre"           -> Theatre
  "Dance & Burlesque" -> Dance
  "Trivia & Gaming"   -> Trivia
  "Markets"           -> Markets
  "Art & Design"      -> Art
  o                   -> Other o