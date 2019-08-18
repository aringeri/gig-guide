{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module GigGuide.Scraper.Beat.Event
  ( EventUrlParams(..)
  , EventCreationError(..)
  , dayParams
  , scrapeEvents
  , fromDB
  , toDB
  ) where

import           Data.Maybe (fromMaybe)
import           Data.List (find, zip, replicate)
import qualified Data.List.NonEmpty as N
import           Control.Applicative((<|>))
import           Control.Monad (forM, mapM)
import           Control.Monad.Except
import           Control.Lens
import           Data.Time (Day, TimeOfDay, fromGregorian)
import           Text.HTML.Scalpel.Core ( Scraper, Selector, (//), atDepth
                                        , hasClass, (@:), textSelector
                                        , tagSelector)
import           Network.Wreq ( Options, get, defaults
                              , params, responseBody)

import           Text.Read (readMaybe)
import           Text.HTML.Scalpel.Class

import           GigGuide.DB (EventCategory(..), EventGenre(..))
import           GigGuide.Types (mkPriceRange', Price, PriceRange, PriceRange(..), priceRange, URL)
import           GigGuide.Types.Range (minVal, maxVal)
import           GigGuide.Types.Event (Event(..))
import qualified GigGuide.DB as DB
import           GigGuide.Scraper.Common

import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Encoding as LE

data EventUrlParams =
  EventUrlParams
    { keywordParam :: T.Text
    , dateParam :: Day
    , radiusParam :: Int
    , eventReload :: Maybe Integer
    , cityLat :: Maybe Double
    , cityLon :: Maybe Double 
    } deriving (Eq, Show)

eventReloadL :: Lens' EventUrlParams (Maybe Integer)
eventReloadL = lens eventReload (\p r -> p { eventReload = r })

itPages :: EventUrlParams -> [EventUrlParams]
itPages = iterate incPage

incPage :: EventUrlParams -> EventUrlParams
incPage = eventReloadL %~ incMaybe

mkOpts :: EventUrlParams -> Options
mkOpts s = defaults & params .~ mkParams s
  where mkParams (EventUrlParams keyword date radius reload _ _) =
          [ ("keyword", keyword)
          , ("date", fmtDate date)
          , ("radius", showt radius)]
          ++ optionL "reload" reload

defaultParams :: EventUrlParams
defaultParams = EventUrlParams 
  ""
  (fromGregorian 2019 8 1) 
  15
  Nothing
  Nothing
  Nothing

dayParams :: Day -> EventUrlParams
dayParams d = defaultParams { dateParam = d }

data EventCreationError = 
    DateParseError
  | PriceParseError
  | PriceRangeParseError
  | ExtraDetailsMissingError
  | TimeParseError
  | VenueUrlMissingError
  deriving (Show, Eq)

scrapeEvents :: URL -> EventUrlParams -> IO [Event]
scrapeEvents u s = printLefts (crawlEvents u s)

crawlEvents :: URL -> EventUrlParams -> IO [Either EventCreationError Event]
crawlEvents u s = do
  let urls = zipUrls u s
  rs <- runScraper urls events
  forM rs (fmap join . mapM scrape)
  where 
    mapNone = fromMaybe (Left ExtraDetailsMissingError)
    scrape = fmap mapNone . scrapeEventDetails

zipUrls :: URL -> EventUrlParams -> [(URL, Options)]
zipUrls u p = 
  zip (replicate maxFetches u)
      (mkOpts <$> itPages p)

type Support = L.Text
data EventParts = EventParts 
  { sourceUrl    :: URL
  , partialEvent :: PartialEvent }

type PartialEvent = TimeOfDay -> 
                    Maybe EventGenre -> 
                    [Support] -> 
                    URL ->
                    Maybe URL ->
                    Event

events :: Scraper L.Text (N.NonEmpty (Either EventCreationError EventParts))
events = do
  es <- chroots' (tagSelector "article") event
  failEmpty es

event :: (MonadError EventCreationError m
         , MonadScraper L.Text m) 
         => m EventParts
event = do
  url <- L.unpack <$> attr "href" "a"
  location <- cleanTexts $
               "div" @: [hasClass "article-card-meta-gig--details"] // textSelector `atDepth` 1 
  price <- liftEither . parsePriceRange =<< text ("span" @: [hasClass "price"])
  
  let nameDiv = "div" @: [hasClass "row"] // "div"
  chroot nameDiv $
    EventParts url <$>
      (Event
         <$>  name
         <*$> url
         <*>  date
         <*>  cats
         <*$> location
         <*$> price
      )
  where name = cleanTexts (textSelector `atDepth` 1)
        date = liftEither . parseDay 
                =<< 
                cleanTexts ("div" @: [hasClass "article-categories"] 
                            // textSelector `atDepth` 1)
        cats = parseEventCategories <$> 
                 cleanTexts ("div" // "div" @: [hasClass "visible-xs"] // textSelector `atDepth` 1)
        
        f <*$> x = f <*> pure x
        infixl 4 <*$>

parseDay :: L.Text -> Either EventCreationError Day
parseDay = parseDayStr . L.unpack

parseDayStr :: String -> Either EventCreationError Day
parseDayStr s = maybe 
  (Left DateParseError)
  Right (parseTimeDefault "%a %d %b %_Y" s)

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

scrapeEventDetails :: MonadIO m => 
                     EventParts  -> 
                     m (Maybe (Either EventCreationError Event))
scrapeEventDetails e = do
  r <- liftIO $ get (sourceUrl e)
  let b = r ^. responseBody
  pure $ runScrapeM 
          (eventDetails $ partialEvent e)
          (LE.decodeUtf8 b) 

eventDetails :: (MonadError EventCreationError m,
                 MonadScraper L.Text m,
                 LiftScraper L.Text m) =>
                PartialEvent -> m Event
eventDetails ev =
  ev <$> timeItem
     <*> genre
     <*> supports
     <*> vUrlItem
     <*> ticketUrl
  where
    findText = findItem textScr
    findItem f' k = findKey k <$> metaItems f'
    findKey k = fmap snd . find ((k==) . fst)
    textScr = fmap L.strip . text
    
    timeItem = orElse TimeParseError . (parseTime =<<) =<< findText "Time"
    genre    = (parseEventGenre =<<)    <$> findText "Genre"
    supports = maybe [] (L.splitOn ",") <$> findText "Support"
    vUrlItem = orElse VenueUrlMissingError 
               . fmap L.unpack =<< findItem url "LOCATION"
    ticketUrl = 
      liftScraper (Just <$> tickets <|> pure Nothing)
    tickets  = L.unpack <$> attr "href" 
                                 ("div" @: [hasClass "article-sidebar"] // "a" `atDepth` 1)
    url s    = chroot s (attr "href" "a")
    orElse e = liftEither . maybe (Left e) Right

parseTime :: L.Text -> Maybe TimeOfDay
parseTime t =
  let str = L.unpack t
  in      parseTimeDefault "%l.%M%p" str
      <|> parseTimeDefault "%l%p"    str


metaItems :: MonadScraper a m => (Selector -> m b) -> m [(a, b)]
metaItems = 
  chroot meta . chroots metaItem . item
  where
    meta     = "div" @: [hasClass "article-meta"]
    metaItem = "div" @: [hasClass "article-meta-item"]
    item sel = do
      k <- text $ "div" @: [hasClass "article-meta-headline"]
      v <- sel  $ "div" @: [hasClass "article-meta-content"]
      pure (k, v)

parseEventGenre :: L.Text -> Maybe EventGenre
parseEventGenre t = 
  case t of
    "Rock"         -> Just Rock
    "Global"       -> Just Global
    "Jazz"         -> Just Jazz
    "Electronic"   -> Just Electronic
    "Hip Hop"      -> Just HipHop
    "Classical"    -> Just Classical
    "R&B"          -> Just RnB
    "Punk"         -> Just Punk
    "Metal"        -> Just Metal
    "Pop"          -> Just Pop
    "Acoustic"     -> Just Acoustic
    "Country/Folk" -> Just CountryFolk
    "Blues"        -> Just Blues
    "Soul/Funk"    -> Just SoulFunk
    "Experimental" -> Just Experimental
    "World Music"  -> Just WorldMusic
    "Indie"        -> Just Indie
    _              -> Nothing

fromDB :: DB.Event -> Event
fromDB e = Event
  (DB.eventName e)
  (DB.eventUrl e)
  (DB.eventDate e)
  (DB.eventCategories e)
  (DB.eventLocation e)
  prFromDB
  (DB.eventTime e)
  (DB.eventGenre e)
  (DB.eventSupports e)
  (DB.eventVenueUrl e)
  (DB.eventTicketUrl e)
  where
    p = DB.eventPrice e
    prFromDB = maybe (Left p)
                     (maybe (Left p) Right . mkPriceRange' p)
                     (DB.eventUpperPrice e)

toDB :: Event -> DB.Event
toDB e = DB.Event
  (eventName e)
  (eventUrl e)
  (eventDate e)
  (eventCategories e)
  (eventLocation e)
  lp
  up
  (eventTime e)
  (eventGenre e)
  (eventSupports e)
  (eventVenueUrl e)
  (eventTicketUrl e)
  where
    lp = either id minPrice (eventPrice e)
    up = either (const Nothing) (Just . maxPrice) (eventPrice e)
    minPrice = minVal . priceRange
    maxPrice = maxVal . priceRange