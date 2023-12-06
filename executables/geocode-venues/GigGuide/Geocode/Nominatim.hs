{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE FlexibleContexts           #-}
module GigGuide.Geocode.Nominatim
  (geocode)
  where

import qualified Data.ByteString.Lazy as LB
import Data.Text ( Text, breakOn, replace, unpack )
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import           Control.Monad.Reader
import           Control.Lens
import           Data.Aeson.Lens
import           Network.Wreq
import           GigGuide.Types.Venue(Venue(..))
import           GigGuide.Types.Geo(Latitude(..), Longitude(..), Coord(..))
import           Text.Read (readMaybe)

geocode :: (MonadIO m) =>
           String -> Venue -> m (Maybe Coord)
geocode url v = do
  let fmt = fmtAddress . address $ v
  liftIO $ geocodeAddress url fmt

geocodeAddress :: String -> Text -> IO (Maybe Coord)
geocodeAddress url t = parseCoord <$> fetchJSON url t

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

removeEsc :: Text -> Text
removeEsc = replace "\n" " " .
            replace "\t" " " .
            replace "\r" " "

parseCoord :: (AsValue p) => p -> Maybe Coord
parseCoord json =
  let parse k = readMaybeT =<< json ^? nth 0 . key k . _String
      lt = Latitude <$> parse "lat"
      ln = Longitude <$> parse "lon"
  in  Coord <$> lt <*> ln

readMaybeT :: Read a => Text -> Maybe a
readMaybeT = readMaybe . unpack

fetchJSON :: String -> Text -> IO LB.ByteString
fetchJSON u t = do
  let opts = defaults & param "q" .~ [t]
                      & param "format" .~ ["json"]
  r <- getWith opts u
  pure $ r ^. responseBody
