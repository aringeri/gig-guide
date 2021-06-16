{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Control.Monad(join)
import Data.Maybe(fromMaybe)
import Data.Text ( Text, isInfixOf )
import Data.Text.Encoding ( decodeUtf8 )
import System.Environment ( lookupEnv )
import Network.Wai
    ( responseLBS,
      Application,
      Request(queryString, rawPathInfo),
      Response )
import Network.HTTP.Types ( status200, status404 )
import Network.Wai.Handler.Warp (run)
import Text.RawString.QQ ( r )

main :: IO ()
main = do
  portEnv <- lookupEnv "PORT"
  let port = fromMaybe "8080" portEnv
  putStrLn $ "Starting geocode stub on port: " ++ port
  run (read port :: Int) app

app :: Application
app request respond = respond $ case rawPathInfo request of
    "/maps/api/geocode/json" -> geocodeGoogle request
    "/search"                -> geocodeNominatim request
    _                        -> notFound

notFound :: Response
notFound = responseLBS
    status404
    [("Content-Type", "text/plain")]
    "404 - Not Found"

data GeocodeRequestType = Google | Nominatim

geocodeGoogle :: Request -> Response
geocodeGoogle request = 
  let query = queryString request
      address = join $ lookup "address" query
  in case address of
    Just a  -> geocodeAddress Google (decodeUtf8 a)
    Nothing -> notFound

geocodeAddress :: GeocodeRequestType -> Text -> Response
geocodeAddress t a
  | "170 Russell St" `isInfixOf` a = respondJson t
  |  otherwise = notFound

respondJson :: GeocodeRequestType -> Response
respondJson Google    = googleResponse
respondJson Nominatim = nominatimResponse

googleResponse :: Response
googleResponse = responseLBS
  status200
  [("Content-Type", "application/json")]
  [r| 
  {
    "results": [
      {
        "geometry": {
          "location": {
            "lat": -37.81194738558151,
            "lng": 144.96788006894667
          }
        }
      }
    ]
  }
  |]

geocodeNominatim :: Request -> Response 
geocodeNominatim request =
  let address = join $ lookup "q" (queryString request)
  in case address of
    Just a -> geocodeAddress Nominatim (decodeUtf8 a)
    Nothing -> notFound

nominatimResponse :: Response
nominatimResponse = responseLBS
  status200
  [("Content-Type", "application/json")]
  [r|
  [
    {
      "place_id": 57958886,
      "licence": "Data OpenStreetMap contributors, ODbL 1.0. https://osm.org/copyright",
      "osm_type": "node",
      "osm_id": 5156152328,
      "boundingbox": [
        "-37.8120609",
        "-37.8119609",
        "144.9678869",
        "144.9679869"
      ],
      "lat": "-37.81194738558151",
      "lon": "144.96788006894667",
      "display_name": "The Billboard, 170, Russell Street, Chinatown, Melbourne, Carlton, Melbourne, City of Melbourne, Victoria, 3000, Australia",
      "class": "amenity",
      "type": "nightclub",
      "importance": 0.6309999999999999
    }
  ]
  |]