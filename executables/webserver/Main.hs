{-# LANGUAGE OverloadedStrings #-}
module Main where

import           System.Environment (getArgs)
import           System.FilePath ((</>))
import           Control.Monad (join)
import           Control.Monad.Reader (runReaderT, MonadReader, MonadIO, liftIO)
import           Data.List (unwords)
import           Text.Printf (printf)
import           Text.Read (readMaybe)
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.Text.Encoding (decodeUtf8)
import           Data.Time.Clock (utctDay, getCurrentTime)
import           Data.Time.Calendar (Day, showGregorian)
import           Data.Time.Format (parseTimeM, defaultTimeLocale)
import           Control.Lens (_2, (%~), (&), (.~), 
                              (^.), to, view)
import           Network.Wai
import           Network.HTTP.Types
import           Network.Wai.Handler.Warp (run)
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
  parseArgs <$> getArgs
    >>= maybe printUsage runApp 

printUsage :: IO ()
printUsage = do
  a <- getArgs
  putStr $ "Invalid arguments: " ++ unwords a ++ "\
    \\n  Usage: server [-c db_connection] [-p server_port] [-f resource_file_path]\n"


parseArgs :: [String] -> Maybe Environment
parseArgs = parseArgs' defaultEnvironment
  where
    parseArgs' e ("-c":d:rest) = 
      parseArgs' (e & dbConfig . connStr .~ d) rest
    parseArgs' e ("-p":p:rest) = do
      pt <- Port <$> readMaybe p
      parseArgs' (e & srvConfig . serverPort .~ pt) rest
    parseArgs' e ("-f":f:rest) = 
      parseArgs' (e & srvConfig . resourceFilePath .~ f) rest
    parseArgs' e [] = Just e
    parseArgs' _ _  = Nothing

runApp :: Environment -> IO ()
runApp e = do
  let p = e ^. serverConfig . serverPort . getPort
  putStrLn $ "Starting server on port: " ++ show p
  run p (app e)

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
mapToEJSON = map toEJSON

toEJSON :: Event -> EventJSON
toEJSON e = EventJSON
  (eventName e)
  (TL.pack . showGregorian . eventDate $ e)
  (fmtPrice . eventPrice $ e)
  (TL.pack . show <$> filter notEmptyCat (eventCategories e))

notEmptyCat :: EventCategory -> Bool
notEmptyCat (Other "") = False
notEmptyCat  _        = True

fmtPrice :: Either Price PriceRange -> TL.Text
fmtPrice (Left 0) = "Free"
fmtPrice (Left d) = TL.pack $ printf "$%.2f" d
fmtPrice (Right (PriceRange range)) =
  TL.pack $ printf "$%.2f - %.2f" (minVal range) 
                                  (maxVal range)
