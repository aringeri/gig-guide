{-# LANGUAGE OverloadedStrings          #-}
module Geocode.Nominatim
  (geocode)
  where

import qualified Data.ByteString.Lazy as LB
import           Data.Text
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import           Control.Concurrent (threadDelay)
import           Control.Monad.Reader
import           Control.Lens
import           Data.Aeson.Lens
import           Network.Wreq
import           GigGuide.Types.Geo (Latitude(..), Longitude(..), Coord(..))
import           GigGuide.DB(Venue(..))
import           GigGuide.Util (readMaybeT)
import           Geocode (removeEsc)

url :: String
url = "https://nominatim.openstreetmap.org/search/"

geocode :: (MonadReader r m, MonadIO m) =>
           Venue -> m (Maybe Coord)
geocode v = liftIO $
    (geocodeT . fmtAddress . venueAddress) v
      <* delay 5

delay :: Int -> IO ()
delay t =
  threadDelay $ t * 1000000 -- 1 second

geocodeT :: Text -> IO (Maybe Coord)
geocodeT t = parseCoord <$> fetchJSON url t

fmtAddress :: L.Text -> Text
fmtAddress = stripSlash
             . removeEsc 
             . L.toStrict

-- strip the leading slash
-- eg. "Level 2/79 Bourke St Melbourne VIC 3000"
stripSlash :: Text -> Text
stripSlash t = 
  case breakOn "/" t of 
    (h,"") -> h
    (_,r)  -> T.drop 1 r


parseCoord :: (AsValue p) => p -> Maybe Coord
parseCoord json =
  let parse k = readMaybeT =<< json ^? nth 0 . key k . _String
      lt = Latitude <$> parse "lat"
      ln = Longitude <$> parse "lon"
  in  Coord <$> lt <*> ln

fetchJSON :: String -> Text -> IO LB.ByteString
fetchJSON u t = do
  let opts = defaults & param "q" .~ [t]
                      & param "format" .~ ["json"]
  r <- getWith opts u
  pure $ r ^. responseBody
