{-# LANGUAGE OverloadedStrings #-}
module GigGuide.DB.DBIntegrationSpec where

import GigGuide.DB.Internal
import GigGuide.DB
import Database.Persist
import Test.Hspec ( describe, it, Spec, runIO, before, afterAll_, shouldMatchList )
import System.Directory ( removeFile )
import Control.Exception ( catch, throwIO )
import System.IO.Error ( isDoesNotExistError )
import Data.Text ( Text, unpack )

dbFile :: Text
dbFile = "integration-test.db"

dbFilePath :: FilePath 
dbFilePath = unpack dbFile

spec :: Spec
spec = do
  afterAll_ (rmDbFile dbFilePath) $ do
    before (rmDbFile dbFilePath) $ do
      describe "when running 'putMany'" $ do
        runIO $ runPutMany dbFile venues
        actualVs <- runIO $ fetchVenues dbFile
        it "should store multiple venues in the db" $ do
          actualVs `shouldMatchList` venues

rmDbFile :: FilePath   -> IO ()
rmDbFile db = do
  putStrLn $ "removing DB file: " ++ db
  removeFile db `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return ()
          | otherwise = throwIO e

venues :: [Venue]
venues = 
  [ Venue 
      { venueName = "170 Russell"
      , venueAddress = "170 Russell St\n                            Melbourne VIC 3000"
      , venueUrl = "https://beat.com.au/directory/170-russell/"
      , venueCategories = [Club, LiveMusic]
      , venueCity = Just "Melbourne, VIC"
      }
    , Venue 
      { venueName = "2 Brothers Brewery"
      , venueAddress = "4 Joyner St\n                            Moorabbin VIC 3189"
      , venueUrl = "https://beat.com.au/directory/2-brothers-brewery/"
      , venueCategories = [LiveMusic, Brewery]
      , venueCity = Just "Moorabbin, VIC"
      }
  ]

fetchVenues :: Text -> IO [Venue]
fetchVenues db = do
  vs <- fetchVenuesEnts db
  return $ entityVal <$> vs

fetchVenuesEnts :: Text -> IO [Entity Venue]
fetchVenuesEnts db =
  runStderrSqlite db $ selectList [] []
