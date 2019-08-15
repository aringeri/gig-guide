{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
module Geocode.Google
 (geocode)
 where

import qualified Data.ByteString.Lazy as LB
import           Data.Text
import qualified Data.Text.Lazy as L
import           Control.Lens
import           Control.Monad.Reader
import           Data.Aeson.Lens
import           Network.Wreq
import           GigGuide.Types.Geo (Latitude(..), Longitude(..), Coord(..))
import           GigGuide.DB(Venue(..))
import           Geocode

url :: String
url = "https://maps.googleapis.com/maps/api/geocode/json"

geocode :: (MonadReader Config m, MonadIO m) => 
           Venue -> m (Maybe Coord)
geocode = geocodeT . fmtAddress

geocodeT :: (MonadReader Config m, MonadIO m) => 
           Text -> m (Maybe Coord)
geocodeT t = parseCoord <$> fetchJSON url t

fmtAddress :: Venue -> Text
fmtAddress v = 
  let (n,a) = (venueName v, venueAddress v)
               & over both L.toStrict
  in  n <> " " <> removeEsc a

parseCoord :: (AsValue p) => p -> Maybe Coord
parseCoord json =
  let parse k = json ^? key "results" 
                      . nth 0 
                      . key "geometry"
                      . key "location"
                      . key k . _Double
      lt = Latitude <$> parse "lat"
      ln = Longitude <$> parse "lng"
  in  Coord <$> lt <*> ln

fetchJSON :: (MonadReader Config m, MonadIO m) => 
            String -> 
            Text -> 
            m LB.ByteString
fetchJSON u a = do
  api <- reader (unApiKey . apiKey)
  let opts = defaults & param "address" .~ [a]
                      & param "key" .~ [api]
  r <- liftIO $ getWith opts u
  pure $ r ^. responseBody
