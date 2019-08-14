module Main where

import           System.Environment (getArgs)
import           Control.Monad (forM_)
import           Data.Text (Text, pack)
import           Data.Time.Format (parseTimeM, defaultTimeLocale)
import           Data.Time.Calendar (Day)
import           GigGuide.Scraper (scrapeEvents, dayParams, toDB)
import           GigGuide.DB (runPutMany)

data Args = Args
 { connStr :: Text
 , startDate :: Day
 , endDay :: Maybe Day
 } deriving (Show, Eq)

main :: IO ()
main = do
  args <- parseArgs <$> getArgs
  case args of
    Just a -> let dr = mkDateRange a
              in  forM_ dr (scrapeAtDate (connStr a))
    _      -> printUsage

printUsage :: IO ()
printUsage = putStr "Unrecognized arguments\
  \\n  Usage : scrape-events db_connection start_date [end_date]\
  \\n\
  \\n  date format   : YYYY-MM-DD\
  \\n  eg connection : \"host=localhost port=5000\"\n"

siteURL :: String
siteURL = "http://www.beat.com.au/gig-guide"

scrapeAtDate :: Text -> Day -> IO ()
scrapeAtDate c d = do
  es <- scrapeEvents siteURL (dayParams d)
  runPutMany c (toDB <$> es)

parseArgs :: [String] -> Maybe Args
parseArgs [conn, date] = 
  Args (pack conn) 
    <$> parseTime date
    <*> pure Nothing
parseArgs [conn, date, endDate] = 
  Args (pack conn) 
    <$> parseTime date
    <*> (Just <$> parseTime endDate)
parseArgs _     = Nothing

parseTime :: String -> Maybe Day
parseTime = parseTimeM True defaultTimeLocale "%Y-%-m-%d"

mkDateRange :: Args -> [Day]
mkDateRange (Args _ s e) = maybe [s] (enumFromTo s) e
