{-# LANGUAGE OverloadedStrings #-}
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

main :: IO ()
main = do
  portEnv <- lookupEnv "PORT"
  let port = fromMaybe "8080" portEnv
  putStrLn $ "Starting geocode stub on port: " ++ port
  run (read port :: Int) app

app :: Application
app request respond = respond $ case rawPathInfo request of
    "/maps/api/geocode/json" -> geocode request
    _                        -> notFound

notFound :: Response
notFound = responseLBS
    status404
    [("Content-Type", "text/plain")]
    "404 - Not Found"

geocode :: Request -> Response
geocode request = 
  let query = queryString request
      address = join $ lookup "address" query
  in case address of
    Just a  -> geocodeAddress (decodeUtf8 a)
    Nothing -> notFound

geocodeAddress :: Text -> Response
geocodeAddress a
  | "170 Russell St" `isInfixOf` a = respondJson
  |  otherwise = notFound

respondJson :: Response
respondJson = responseLBS
  status200
  [("Content-Type", "application/json")]
  "{\
  \  \"results\": [\
  \     {\
  \       \"geometry\": {\
  \         \"location\": {\
  \           \"lat\": -37.81194738558151, \
  \           \"lng\": 144.96788006894667 \
  \         }\
  \       }\
  \     }\
  \  ]\
  \}"