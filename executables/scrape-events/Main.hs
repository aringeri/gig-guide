{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Data.Time
import GigGuide.Types ( URL, Price, PriceRange, PriceRange(..) )
import Options.Applicative
import GigGuide.Scraper.Common (fetchPage)
import GigGuide.Scraper.BeatScraper
    ( eventSearchDayParam,
      mkOpts,
      eventOverviews,
      eventDetail,
      iteratePages,
      EventSearchParams(..),
      EventCreationError(..) )
import GigGuide.Types.EventOverview
    ( EventOverview(..), EventCategory, EventCategory(..) )
import GigGuide.Types.EventDetails
    ( EventDetails(..), EventGenre, EventGenre(..) )
import GigGuide.Types.Venue ( Venue(name) )
import qualified GigGuide.Types.Venue as Venue
import GigGuide.Types.VenueAndGeo
    ( VenueAndGeo, VenueAndGeo(venue), VenueAndGeo(..) )
import GigGuide.Types.Geo
    ( Coord(..), Longitude(..), Latitude(..) )
import GigGuide.Types.Range (MinMaxRange(..))
import GigGuide.Types.GeoJSON (FeatureCollection (FeatureCollection), Feature (Feature), Geometry (Geometry), Properties (Properties), EventJSON (EventJSON))

import Data.Aeson (encodeFile, decodeFileStrict)
import Text.HTML.Scalpel (scrapeStringLikeT)
import Network.Wreq (defaults)

import qualified Data.Text.Lazy.Encoding as LE
import Text.Printf (printf)

import Data.Maybe (catMaybes, fromMaybe, isNothing)

import Control.Monad (when, join)
import Data.Foldable (for_)
import Data.Bifunctor (Bifunctor (first))
import Control.Lens ((<&>))

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Multimap (Multimap)
import qualified Data.Multimap as MM
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Lazy as T
import qualified Data.Text as Text
import Control.Monad.Logger (MonadLogger, logWarnN, runStdoutLoggingT, logInfoN)
import Control.Monad.Trans (MonadIO (liftIO))

data Args = Args
  {
    date :: Day,
    venueJsonFile :: FilePath,
    outFile :: FilePath,
    url :: URL,
    maxFetches :: Int
  } deriving(Show)

main :: IO()
main = do
    runStdoutLoggingT app

app :: (MonadLogger m, MonadIO m) => m ()
app = do
  a@(Args day _ outF url maxF) <- liftIO getArgs
  let opt = eventSearchDayParam day
  let opts = take maxF $ iteratePages opt

  venues' <- loadVenues a
  when (isNothing venues')
    (logWarnN $ "WARN: Unable to decode input venue file: " `mappend` Text.pack (show a))

  for_ venues' (\venues -> do
      events <- join <$> seqUntilNothing (scrapeFullEvent url <$> opts)
      m <- groupByVenue venues events
      let fc = convertToGeoJSON m
      liftIO $ encodeFile outF fc
    )

loadVenues :: (MonadLogger m, MonadIO m) => Args -> m (Maybe (Map URL VenueAndGeo))
loadVenues a = do
  (x :: Maybe [VenueAndGeo]) <- liftIO $ decodeFileStrict (venueJsonFile a)
  return $ fmap (M.fromList . fmap (\v -> (Venue.url (venue v),v)) ) x

groupByVenue :: MonadLogger m => Map URL VenueAndGeo -> [FullEvent] -> m (Multimap VenueAndGeo FullEvent)
groupByVenue vs es = MM.fromList . catMaybes <$> lookupVenues es
  where
    lookupVenues = mapM (\e -> (fmap . fmap) (,e) (findVenue e))

    findVenue :: MonadLogger x => FullEvent -> x (Maybe VenueAndGeo)
    findVenue e =
      let venueUrl = eventVenueUrl . details $ e
      in logIfNotFound e (M.lookup venueUrl vs)
    logIfNotFound e m = case m of
      Nothing -> logWarnN ("Cannot map event to venue: " `mappend` Text.pack (show e))
        >> return Nothing
      j -> return j


convertToGeoJSON :: Multimap VenueAndGeo FullEvent -> FeatureCollection
convertToGeoJSON = FeatureCollection
    . fmap convertToGeoFeature
    . (M.toList . MM.toMap)
  where
    convertToGeoFeature :: (VenueAndGeo, NonEmpty FullEvent) -> Feature
    convertToGeoFeature (VenueAndGeo v g, es) =
      Feature "Feature"
        (Geometry "Point" [getLon $ lon g, getLat $ lat g])
        (Properties (name v) (toEventJSONs es))

    toEventJSONs = NE.toList
      . fmap toEventJSON
      . NE.sortWith (eventTime . details)

    toEventJSON :: FullEvent -> EventJSON
    toEventJSON =
      EventJSON <$> (eventName . overview)
        <*> (T.pack . showGregorian . eventDate . overview)
        <*> (fmtPrice . eventPrice . overview)
        <*> (fmap (T.pack . show)
              . filter notEmptyCat
              . eventCategories . overview)
        <*> (fmtTime . eventTime . details)
        <*> (fmap fmtGenre . eventGenre . details)
        <*> (eventSupports . details)
        <*> (eventTicketUrl . details)

    notEmptyCat :: EventCategory -> Bool
    notEmptyCat (Other "") = False
    notEmptyCat  _        = True

    fmtPrice :: Either Price PriceRange -> T.Text
    fmtPrice (Left 0) = "Free"
    fmtPrice (Left d) = T.pack $ printf "$%.2f" d
    fmtPrice (Right (PriceRange range)) =
      T.pack $ printf "$%.2f - %.2f" (minVal range)
                                      (maxVal range)

    fmtTime :: TimeOfDay -> String
    fmtTime t =
      let fmt = if todMin t == 0
                  then "%l%P"
                  else "%l:%M%P"
      in formatTime defaultTimeLocale fmt t

    fmtGenre :: EventGenre -> T.Text
    fmtGenre g =
      case g of
        Rock         -> "Rock"
        Global       -> "Global"
        Jazz         -> "Jazz"
        Electronic   -> "Electronic"
        HipHop       -> "Hip Hop"
        Classical    -> "Classical"
        RnB          -> "R&B"
        Punk         -> "Punk"
        Metal        -> "Metal"
        Pop          -> "Pop"
        Acoustic     -> "Acoustic"
        CountryFolk  -> "Country/Folk"
        Blues        -> "Blues"
        SoulFunk     -> "Soul/Funk"
        Experimental -> "Experimental"
        WorldMusic   -> "World Music"
        Indie        -> "Indie"

data FullEvent = FullEvent
  { overview :: EventOverview
  , details :: EventDetails
  } deriving (Show, Eq)

scrapeFullEvent :: (MonadLogger m, MonadIO m) => URL -> EventSearchParams -> m (Maybe [FullEvent])
scrapeFullEvent url o = do
  logInfoN $ "fetching page " `mappend` Text.pack (show (fromMaybe 0 (eventReload o)))
  page <- LE.decodeUtf8 <$> liftIO (fetchPage url (mkOpts o))
  overviews <- scrapeStringLikeT page eventOverviews
    <&> fmap (filter (either (const True) (hasSameDay o)))

  mkFullEvent overviews

type OverviewScrapeResult = Maybe [Either EventCreationError EventOverview]

mkFullEvent :: (MonadLogger m, MonadIO m) => OverviewScrapeResult -> m (Maybe [FullEvent])
mkFullEvent r = do
  seqFullEvents <- mapM (printErrsAndMapM mkEventDetails) r
  mapM (printErrsAndMapM pure) seqFullEvents

printErrsAndMapM :: MonadLogger m => (a -> m b) -> [Either EventCreationError a] -> m [b]
printErrsAndMapM = consumeLsMapRs (logWarnN . Text.pack . show)

consumeLsMapRs :: Monad m => (e -> m ()) -> (a -> m b) -> [Either e a] -> m[b]
consumeLsMapRs f g es = do
  catMaybes <$> mapM mapEither es
  where
    consumeL l = Nothing <$ f l
    mapR = fmap Just . g
    mapEither = either consumeL mapR

mkEventDetails :: (MonadLogger m, MonadIO m) => EventOverview -> m (Either EventCreationError FullEvent)
mkEventDetails overview = do
  logInfoN $ "Scraping event details for: " `mappend` Text.pack (show (eventName overview))
  let url = eventUrl overview
  detailPage <- LE.decodeUtf8 <$> liftIO (fetchPage url defaults)
  detail <- join . maybeToMissingError <$> scrapeStringLikeT detailPage eventDetail
  return $ FullEvent overview <$> addContext url detail
  where
    maybeToMissingError = maybe (Left ExtraDetailsMissingError) Right
    addContext u = first (ErrorContext ("parsing event details, url = " ++ u))

seqUntilNothing :: Monad m => [m (Maybe a)] -> m [a]
seqUntilNothing [] = pure []
seqUntilNothing (a:as) = do
  v <- a
  case v of
    Just v' -> (:) v' <$> seqUntilNothing as
    _ -> pure []

takeWhileM :: Monad m => (a -> Bool) -> [m a] -> m [a]
takeWhileM _ [] = return []
takeWhileM p (a:as) = do
  a' <- a
  if p a'
    then (:) a' <$> takeWhileM p as
    else return []

hasSameDay :: EventSearchParams -> EventOverview -> Bool
hasSameDay params ev = dateParam params == eventDate ev

getArgs :: IO Args
getArgs =
  execParser parserInfo

parserInfo :: ParserInfo Args
parserInfo =
  info (parseArgs <**> helper)
     (fullDesc
      <> progDesc "Geocode venue addresses from beat.com.au"
      <> header "geocode-venues"
     )

parseArgs :: Parser Args
parseArgs = Args
  <$> dateOption
     (short 'd'
     <> long "date"
     <> help "Date to search for events (in format: YYYY-MM-DD)")
  <*> strOption
    (short 'i'
    <> long "inputVenues"
    <> help "Input file containing a list of geocoded venues as JSON"
    )
  <*> strOption
    (short 'o'
    <> long "outFile"
    <> help "Output file to write the event overview as JSON"
    )
  <*> strOption
    (short 'u'
    <> long "url"
    <> value "http://www.beat.com.au/gig-guide"
    <> help "URL to retrieve event data. Defaults to 'http://www.beat.com.au/gig-guide'")
  <*> option auto
    (short 'm'
    <> long "maxFetches"
    <> value 250)

dateOption :: Mod OptionFields Day -> Parser Day
dateOption = option (maybeReader parseTime)

parseTime :: String -> Maybe Day
parseTime = parseTimeM True defaultTimeLocale "%Y-%-m-%d"