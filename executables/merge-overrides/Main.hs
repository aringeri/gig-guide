{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant flip" #-}
module Main where

import Options.Applicative
import Data.Aeson ( decodeFileStrict, encodeFile )

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (ReaderT (runReaderT), MonadReader, asks)
import Control.Monad.Except (ExceptT, runExceptT, MonadError, liftEither)
import Control.Monad.Logger (MonadLogger, logErrorN, runStdoutLoggingT, LoggingT, logInfoN)

import qualified Data.Text as T
import Data.Map (Map, fromList, traverseWithKey)
import qualified Data.Map as M

import GigGuide.Types (URL)
import GigGuide.Scraper.Common (showt)
import GigGuide.Types.Venue (Venue (url))
import GigGuide.Types.VenueAndGeo (VenueAndGeo(..))
import GigGuide.Types.Override (VenueOverride (venueOverride))
import qualified GigGuide.Types.Override as Override
import Text.Printf (printf)

newtype RawVenues = RawVenues FilePath deriving (Show, Eq, Read)
newtype GeocodedVenues = GeocodedVenues FilePath deriving (Show, Eq, Read)
newtype Overrides = Overrides FilePath deriving (Show, Eq, Read)
newtype OutputFile = OutputFile{ outputFilePath :: FilePath } deriving (Show, Eq, Read)

data Args = Args
    { raw :: RawVenues
    , geocoded :: GeocodedVenues
    , overrides :: Overrides
    , output :: OutputFile
    } deriving (Show, Eq, Read)

data Error where
  UnableToDecodeFile :: FilePath -> Error
  OverrideURLNotFound :: URL -> Error
  deriving (Show, Eq)

main :: IO ()
main = do
    args <- getArgs
    r <- run app args
    runStdoutLoggingT (logError r)

logError :: (Show e, MonadLogger m) => Either e a -> m ()
logError = either (logErrorN . showt)  (const (pure ()))

run :: ExceptT Error (ReaderT Args (LoggingT IO)) a -> Args -> IO (Either Error a)
run m a = runStdoutLoggingT (runReaderT (runExceptT m) a)

app :: (MonadLogger m, MonadReader Args m, MonadIO m, MonadError Error m) => m ()
app = do
    allVenues <- decodeRawVenues
    geocodedVenues <- decodeGeocodedVenues

    logInfoN . T.pack $ printf "Starting with %d geocoded venues" (M.size geocodedVenues)

    vOverrides <- decodeVenueOverrides
    logInfoN . T.pack $ printf "Overriding properties of %d venues" (M.size vOverrides)

    overridden <- flip traverseWithKey vOverrides
      (\k a -> do
        let newGeo = Override.geo (venueOverride a)
        let updateGeocoded = (\x -> x{ geo = newGeo }) <$> M.lookup k geocodedVenues
        let updateRaw      = (`VenueAndGeo` newGeo)    <$> M.lookup k allVenues

        liftEither $ maybeToEither (OverrideURLNotFound k) (updateGeocoded <|> updateRaw)
      )

    let merged = M.elems $ overridden <> geocodedVenues
    logInfoN . T.pack $ printf "Final number of venues: %d" (length merged)

    (OutputFile out) <- asks output
    liftIO $ encodeFile out merged

decodeRawVenues :: (MonadReader Args m, MonadIO m, MonadError Error m) => m (Map URL Venue)
decodeRawVenues = do
  (RawVenues file) <- asks raw
  (mvs :: Maybe [Venue]) <- liftIO $ decodeFileStrict file
  vs <- liftEither $ maybeToEither (UnableToDecodeFile file) mvs
  return $ asMap url vs

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e = maybe (Left e) Right

asMap :: Ord k => (a -> k) -> [a] -> Map k a
asMap f = fromList . fmap (\a -> (f a, a))

decodeGeocodedVenues :: (MonadReader Args m, MonadIO m, MonadError Error m)
  => m (Map URL VenueAndGeo)
decodeGeocodedVenues = do
  (GeocodedVenues file) <- asks geocoded
  (mvs :: Maybe [VenueAndGeo]) <- liftIO $ decodeFileStrict file
  vs <- liftEither $ maybeToEither (UnableToDecodeFile file) mvs
  return $ asMap (url . venue) vs

decodeVenueOverrides :: (MonadReader Args m, MonadIO m, MonadError Error m)
  => m (Map URL VenueOverride)
decodeVenueOverrides = do
  (Overrides file) <- asks overrides
  (mvs :: Maybe [VenueOverride]) <- liftIO $ decodeFileStrict file
  vs <- liftEither $ maybeToEither (UnableToDecodeFile file) mvs
  return $ asMap Override.url vs

getArgs :: IO Args
getArgs =
  execParser parserInfo

parserInfo :: ParserInfo Args
parserInfo =
  info (parseArgs <**> helper)
     (fullDesc
      <> progDesc "Manual overrides for venue data"
      <> header "merge-overrides"
     )

parseArgs :: Parser Args
parseArgs =
  Args
    <$> ( RawVenues
            <$> strOption
              ( short 'r'
                  <> long "raw-venues"
                  <> help "Input file containing the un-geocoded venues as JSON"
              )
        )
    <*> ( GeocodedVenues
            <$> strOption
              ( short 'g'
                  <> long "geocoded-venues"
                  <> help "Input file containing the geocoded venues as JSON"
              )
        )
    <*> ( Overrides
            <$> strOption
              ( short 'i'
                  <> long "overrides"
                  <> help "Input file containing the venue properties to merge with existing data"
              )
        )
    <*> ( OutputFile
            <$> strOption
              ( short 'o'
                  <> long "output"
                  <> help "Output file to place merged data"
              )
        )