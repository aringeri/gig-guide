{-# LANGUAGE OverloadedStrings #-}
module Main where

import           System.Environment (getArgs)
import           System.FilePath ((</>))
import           Control.Monad (join)
import           Control.Monad.Reader (runReaderT, MonadReader, MonadIO, liftIO)
import           Data.List (unwords, sortOn)
import           Data.Maybe (maybe)
import           Text.Printf (printf)
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.Text.Encoding (decodeUtf8)
import           Data.Time (TimeOfDay, formatTime, todMin)
import           Data.Time.Clock (utctDay, getCurrentTime)
import           Data.Time.Calendar (Day, showGregorian)
import           Data.Time.Format (parseTimeM, defaultTimeLocale)
import           Options.Applicative
import           Control.Lens (_2, (%~), 
                              (^.), to, view)
import           Network.Wai
import           Network.HTTP.Types
import           Network.Wai.Handler.Warp (run, setPort, defaultSettings)
import           Network.Wai.Handler.WarpTLS (runTLS, tlsSettings)
import           Data.Aeson

import           GigGuide.DB hiding (Event(..))
import           GigGuide.Scraper.BeatScraper (fromDB)
import           GigGuide.Util (readMaybeT)
import           GigGuide.Types.Geo (Latitude(..), Longitude(..), Coord(..), mkBounds, CoordBounds)
import           GigGuide.Types (Price, PriceRange(..))
import           GigGuide.Types.Event (Event(..))
import           GigGuide.Types.Range (minVal, maxVal)
import           GigGuide.Types.GeoJSON (FeatureCollection(..), Feature(..), EventJSON(..),
                                    Geometry(..), Properties(..))
import           GigGuide.Config
import           GigGuide.AppM

type ContentType = B.ByteString

main :: IO ()
main = 
  runApp =<< execParser parserInfo

printUsage :: IO ()
printUsage = do
  a <- getArgs
  putStr $ "Invalid arguments: " ++ unwords a ++ "\
    \\n  Usage: server [-c db_connection] [-p server_port] [-f resource_file_path]\n"

parserInfo :: ParserInfo Environment
parserInfo =
  info (parseArgs <**> helper)
     (fullDesc
      <> progDesc "GigGuide - Web server"
      <> header "webserver"
     )

parseArgs :: Parser Environment
parseArgs = Environment
  <$> (DBConfig <$> parseConnection)
  <*> (ServerConfig
       <$> (Port <$> parsePort)
       <*> parseFilePath
       <*> (Just <$> parseTls <|> pure Nothing)
      )
  where
    parseConnection = strOption
           (short 'c'
         <> long "db_connection"
         <> help "Connection string for database containing gig info (default 'host=localhost port=5454')"
         <> value "host=localhost port=5454")
    parseFilePath = strOption
           (short 'f'
         <> long "file_path_resource"
         <> help "Directory containing the web resources/assets. (default '.'')"
         <> value ".")
    parsePort = option auto
           (short 'p'
         <> long "port"
         <> help "The port to run the server on. (default 80)"
         <> value 80)
    parseTls = TlsConfig
      <$> strOption
           (short 't'
         <> long "tls_certificate"
         <> help "Path to TLS certificate file")
      <*> strOption
           (short 'k'
         <> long "tls_key"
         <> help "Path to file containing TLS key")

runApp :: Environment -> IO ()
runApp e = do
  let p      = e ^. serverConfig . serverPort . getPort
      tlsCfg = e ^. serverConfig . serverTlsConfig
  putStrLn $ "Starting server on port: " ++ show p
  maybe (runHttp p)
        (runHttps p)
        tlsCfg
  where
    runHttp  p     = run p (app e)
    runHttps p tls = runTLS (tlsSettings (_certificateFile tls) (_keyFile tls))
                       (setPort p defaultSettings)
                       (app e)

app :: Environment -> Application
app env request respond =
  respond =<< runReaderT (unApp (handleRequest request)) env

handleRequest :: (MonadIO m, MonadReader r m, HasDBConfig r, HasServerConfig r) =>
                 Request -> m Response
handleRequest request =
  case pathInfo request of
    []           -> staticFileM "index.html" "text/html"
    ["index.js"] -> staticFileM "index.js"   "application/javascript"
    ["search"]   -> search request
    ["icon", p]  -> staticFileM ("icons/" ++ T.unpack p) "image/png"
    _            -> pure notFound

staticFileM :: (MonadReader r m, HasServerConfig r) => 
               FilePath -> ContentType -> m Response
staticFileM f c = do
  r <- view (serverConfig . resourceFilePath) 
  pure $ staticFile (r </> f) c

staticFile :: FilePath -> ContentType -> Response
staticFile f c = responseFile
    status200
    [("Content-Type", c)]
    f
    Nothing

search :: (MonadIO m, MonadReader r m, HasDBConfig r) =>
          Request -> m Response
search r =
  let parseDate = readDayParam r "date"
      parseCoord latK lonK = Coord
        <$> (Latitude  <$> readParam r latK)
        <*> (Longitude <$> readParam r lonK)
      bounds = join $ 
        mkBounds <$> parseCoord "south" "west" 
                 <*> parseCoord "north" "east"
      today = liftIO $ utctDay <$> getCurrentTime
  in case bounds of
      Just b -> do
          day <- maybe today pure parseDate
          respondJSON <$> selectEvents day b
      _  -> pure notFound

queryParam :: Request -> B.ByteString -> Maybe B.ByteString
queryParam r k = join (lookup k $ queryString r)

parseParam :: (B.ByteString -> Maybe a) -> Request -> B.ByteString -> Maybe a
parseParam parse r k = parse =<< queryParam r k

readParam :: Read a => Request -> B.ByteString -> Maybe a
readParam = parseParam (readMaybeT . decodeUtf8)

readDayParam :: Request -> B.ByteString -> Maybe Day
readDayParam = parseParam (parseTime . T.unpack . decodeUtf8)
  where parseTime = parseTimeM True 
                               defaultTimeLocale
                               "%Y-%-m-%-d"

selectEvents :: (MonadIO m, MonadReader r m, HasDBConfig r) =>
                Day -> CoordBounds -> m FeatureCollection
selectEvents day bounds = do
  conn     <- view (dBConfig . connStr . to T.pack)
  dbEvents <- liftIO $ selectEventsByVenue conn day bounds
  let es = (_2 %~ fmap fromDB) <$> dbEvents
  pure $ toFeatureCollection es

respondJSON :: ToJSON a => a -> Response
respondJSON = responseLBS
            status200
            [("Content-Type", "text/json")]
            . encode

notFound :: Response
notFound = responseLBS
    status404
    [("Content-Type", "text/plain")]
    "404 - Not Found"

toFeatureCollection :: [((VenueLocation, Venue), [Event])] -> FeatureCollection
toFeatureCollection = 
  FeatureCollection . fmap toFeature

toFeature :: ((VenueLocation, Venue), [Event]) -> Feature
toFeature ((VenueLocation (Latitude lat') (Longitude lon'), v), e) = 
  Feature "Feature" 
          (Geometry "Point" [lon', lat'])
          (Properties (venueName v) (mapToEJSON e))

mapToEJSON :: [Event] -> [EventJSON]
mapToEJSON = map toEJSON . sortOn eventTime

toEJSON :: Event -> EventJSON
toEJSON e = EventJSON
  (eventName e)
  (TL.pack . showGregorian . eventDate $ e)
  (fmtPrice . eventPrice $ e)
  (TL.pack . show <$> filter notEmptyCat (eventCategories e))
  (fmtTime . eventTime $ e)
  (fmtGenre <$> eventGenre e)
  (eventSupports e)
  (eventTicketUrl e)

notEmptyCat :: EventCategory -> Bool
notEmptyCat (Other "") = False
notEmptyCat  _        = True

fmtPrice :: Either Price PriceRange -> TL.Text
fmtPrice (Left 0) = "Free"
fmtPrice (Left d) = TL.pack $ printf "$%.2f" d
fmtPrice (Right (PriceRange range)) =
  TL.pack $ printf "$%.2f - %.2f" (minVal range) 
                                  (maxVal range)

fmtTime :: TimeOfDay -> String
fmtTime t =
  let fmt = if todMin t == 0
              then "%l%P"
              else "%l:%M%P"
  in formatTime defaultTimeLocale fmt t

fmtGenre :: EventGenre -> TL.Text
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
