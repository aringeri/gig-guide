module Main where

import System.Environment (getArgs)
import Data.Text (pack)
import GigGuide.Scraper (scrapeVenues, venueSearchParams)
import GigGuide.DB (runPutMany)

main :: IO ()
main = do
  [connStr] <- getArgs
  venues <- scrapeVenues
      "http://www.beat.com.au/directory"
      venueSearchParams 
  runPutMany (pack connStr) venues
