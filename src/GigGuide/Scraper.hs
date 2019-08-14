{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module GigGuide.Scraper where

import           Data.Char (isSpace)
import           Data.Maybe (fromMaybe)
import           Data.Either (partitionEithers)
import           Control.Monad (guard)
import           Control.Monad.Except hiding (liftEither)
import           Control.Lens
import           Data.Time (Day, fromGregorian, defaultTimeLocale)
import           Data.Time.Format (formatTime, parseTimeM)
import           Text.HTML.Scalpel.Core ( Scraper, Selector, (//), atDepth
                                        , hasClass, (@:), textSelector
                                        , tagSelector, scrapeStringLike)
import           Network.Wreq

import           Text.Read (readMaybe)
import           Text.HTML.Scalpel.Class

import           GigGuide.DB (Venue(..), VenueCategory(..), EventCategory(..))
import           GigGuide.Types (mkPriceRange', Price, PriceRange, PriceRange(..), priceRange, URL)
import           GigGuide.Types.Range (minVal, maxVal)
import           GigGuide.Types.Event (Event(..))
import qualified GigGuide.DB as DB


import qualified Data.Set as Set
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

fromDB :: DB.Event -> Event
fromDB e = Event
  (DB.eventName e)
  (DB.eventUrl e)
  (DB.eventDate e)
  (DB.eventCategories e)
  (DB.eventLocation e)
  prFromDB
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
  where
    lp = either id minPrice (eventPrice e)
    up = either (const Nothing) (Just . maxPrice) (eventPrice e)
    minPrice = minVal . priceRange
    maxPrice = maxVal . priceRange

data EventCreationError = 
    DateParseError
  | PriceParseError
  | PriceRangeParseError
  deriving (Show, Eq)

data VenueCreationError = Blank
  deriving (Show, Eq)

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

scrapeEvents' :: URL -> SearchParams -> IO (Set.Set Event)
scrapeEvents' u p = Set.fromList <$> scrapeEvents u p

scrapeEvents :: URL -> SearchParams -> IO [Event]
scrapeEvents = runErrorScraper events

scrapeVenues :: URL -> SearchParams -> IO [Venue]
scrapeVenues = runErrorScraper venues

runErrorScraper :: (Show e, Ord a) => Scraper L.Text [Either e a] -> URL -> SearchParams -> IO [a]
runErrorScraper scr u s = do
  scraped <- scrapeAll u s scr
  let (err,r) = partitionEithers (concat scraped) 
  mapM_ print err
  pure r

maxFetches :: Int
maxFetches = 250

scrapeAll :: {-Show a =>-} URL -> SearchParams -> Scraper L.Text a -> IO [a]
scrapeAll url p scr =
  run (take maxFetches $ iterate incReload p)
  where incReload s = s { reloadParam = Just $ maybe 1 (+1) (reloadParam s) } 
        run []     = return []
        run (a:as) = do
          c <- fetchPage url a
          --putStrLn (maybe "" show $ reloadParam a)
          case scrapeStringLike (LE.decodeUtf8 c) scr of
            Just e ->
              --putStrLn $ show e
              (e :) <$> run as
            _      -> return []


testFile :: String -> Scraper L.Text a -> IO (Maybe a)
testFile f sc = do 
  s <- readFile f
  --return $ runScrapeM s art
  return $ scrapeStringLike (L.pack s) sc

events :: Scraper L.Text [Either EventCreationError Event] 
events = do
  es <- chroots' (tagSelector "article") event
  guard $ not (null es)
  return es

-- available in mtl 2.2.2
liftEither :: MonadError e m => Either e a -> m a
liftEither = either throwError return

event :: (MonadError EventCreationError m
         , MonadScraper L.Text m) 
         => m Event
event = do
  url <- L.unpack <$> attr "href" "a"
  location <- cleanTexts $
               "div" @: [hasClass "article-card-meta-gig--details"] // textSelector `atDepth` 1 
  price <- liftEither . parsePriceRange =<< text ("span" @: [hasClass "price"])
  
  let nameDiv = "div" @: [hasClass "row"] // "div"
  chroot nameDiv
    (Event <$> name
           <-*> url
           <*> date
           <*> cats
           <-*> location
           <-*> price
    )
  where name = cleanTexts (textSelector `atDepth` 1)
        date = liftEither . parseDay 
                =<< 
                cleanTexts ("div" @: [hasClass "article-categories"] 
                            // textSelector `atDepth` 1)
        cats = parseEventCategories <$> 
                 cleanTexts ("div" // "div" @: [hasClass "visible-xs"] // textSelector `atDepth` 1)
        
        f <-*> x = f <*> pure x
        infixl 4 <-*>

parseDay :: L.Text -> Either EventCreationError Day
parseDay = parseDayStr . L.unpack

parseDayStr :: String -> Either EventCreationError Day
parseDayStr s = maybe 
  (Left DateParseError)
  Right (parseTimeM True defaultTimeLocale "%a %d %b %_Y" s)

parseEventCategories :: L.Text -> [EventCategory]
parseEventCategories = parseCommaSep parseEventCategory

parseCommaSep :: (L.Text -> a) -> L.Text -> [a]
parseCommaSep f t = f . L.strip <$> L.splitOn "," t

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
 
cleanTexts :: (MonadScraper L.Text m) => Selector -> m L.Text
cleanTexts = fmap (L.strip . joinEmpty) . texts

joinEmpty :: [L.Text] -> L.Text
joinEmpty = L.concat . filter (L.any $ not . isSpace)

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
  chroot ("div" @: [hasClass "row"]) 
    (Venue 
      <$> name
      <*> address
      <*> pure url
      <*> categories
      <*> city
    )
  where 
    name = cleanTexts ("div" // "div" // textSelector `atDepth` 1)
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
