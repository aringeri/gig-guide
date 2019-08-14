{-# LANGUAGE OverloadedStrings          #-}
module Main where

import           Control.Concurrent (threadDelay)
import           Control.Monad (filterM)
import           System.Environment (getArgs)
import           Data.Text (Text, pack, unpack)
import qualified Data.Text      as T
import qualified Data.Text.Lazy as L
import           Database.Persist
import           GigGuide.DB ( migrateVenueGeos, insertVenueCoords
                        , Venue(..), EntityField(..), selectAll
                        , runStderrSqlite)
import           GigGuide.Types.Geo (Coord)
import           Geocode

type ConnStr = Text

main :: IO ()
main = fmap parseArgs getArgs
    >>= maybe printUsage runGeocoder

runGeocoder :: ConnStr -> IO ()
runGeocoder cstr = do
  venues <- selectAll cstr :: IO [Entity Venue]
  if not (null venues) 
    then do
      migrateVenueGeos cstr
      sequence_ $ lookupAndInsert cstr <$> venues
    else putStrLn $ "Warning : No Venues Found in db \"" 
                    ++ unpack cstr ++ "\""

parseArgs :: [String] -> Maybe ConnStr
parseArgs [conn] = Just $ pack conn
parseArgs _      = Nothing

printUsage :: IO ()
printUsage = putStr "Unrecognized arguments\
  \\n  Usage : geocode-venues db_connection\
  \\n\
  \\neg connection : \"host=localhost port=5000\"\n"

retryVenuesWithoutGeo :: Text -> IO ()
retryVenuesWithoutGeo t = do
  vs <- selectVenuesWithoutGeo t
  sequence_ (lookupAndInsert t <$> vs)

selectVenuesWithoutGeo :: Text -> IO [Entity Venue]
selectVenuesWithoutGeo cstr = do
  vs <- selectAll cstr :: IO [Entity Venue]
  filterM
     (\v -> (==0) <$> runStderrSqlite cstr (count [VenueGeoVenueId ==. entityKey v]))
     vs

lookupAndInsert :: Text -> Entity Venue -> IO ()
lookupAndInsert cstr v = do
  c <- lookupCoords v
  logOrInsert cstr c
  delay 5

lookupCoords :: Entity Venue -> IO (Key Venue, Either String Coord)
lookupCoords e = do
  coords <- geocode (fmtAddress . venueAddress . entityVal $ e)
  pure
    (entityKey e
    , maybe (Left $ "Geocoding failed for: " ++ show e)
            Right
            coords)

fmtAddress :: L.Text -> Text
fmtAddress = stripSlash
             . removeEsc 
             . L.toStrict

-- strip the leading slash
-- eg. "Level 2/79 Bourke St Melbourne VIC 3000"
stripSlash :: Text -> Text
stripSlash t = 
  case T.breakOn "/" t of 
    (h,"") -> h
    (_,r)  -> T.drop 1 r

removeEsc :: Text -> Text
removeEsc = T.replace "\n" " " .
            T.replace "\t" " " .
            T.replace "\r" " "

delay :: Int -> IO ()
delay t =
  threadDelay $ t * 1000000 -- 1 second

logOrInsert :: Text -> (Key Venue, Either String Coord) -> IO ()
logOrInsert conn coords =
  case coords of
    (v, Right c) -> insertVenueCoords conn c v
    (_, Left e)  -> putStrLn e
