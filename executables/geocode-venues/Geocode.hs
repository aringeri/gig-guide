{-# LANGUAGE OverloadedStrings          #-}
module Geocode
  ( geocode )
where

import qualified Data.ByteString.Lazy as LB
import           Data.Text
import           Control.Lens
import           Data.Aeson.Lens
import           Network.Wreq
import           GigGuide.Types.Geo (Latitude(..), Longitude(..), Coord(..))
import           GigGuide.Util (readMaybeT)

url :: String
url = "https://nominatim.openstreetmap.org/search/"

geocode :: Text -> IO (Maybe Coord)
geocode t = parseCoord <$> fetchJSON url t

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
