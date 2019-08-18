module Main where

import           Data.Either (partitionEithers)
import           Control.Monad (forM_, when)
import           Data.Text (Text)
import           Data.Time.Format (parseTimeM, defaultTimeLocale)
import           Data.Time.Calendar (Day)
import           GigGuide.Scraper.BeatScraper (scrapeEvents, dayParams, toDB)
import           GigGuide.DB (runPutMany, migrateEntity, Event(..))

import           Options.Applicative
import           Data.Semigroup((<>))

data Args = Args
 { connStr :: Text
 , startDate :: Day
 , endDay :: Maybe Day
 , migrateDb :: Bool
 } deriving (Show, Eq)

main :: IO ()
main =
  run =<< execParser parserInfo

run :: Args -> IO ()
run a = do
  let dr = mkDateRange a

  when (migrateDb a) $ do
    putStrLn "Migrating event table."
    migrateEntity (connStr a) (Nothing :: Maybe Event)
  forM_ dr (scrapeAtDate (connStr a))

siteURL :: String
siteURL = "http://www.beat.com.au/gig-guide"

scrapeAtDate :: Text -> Day -> IO ()
scrapeAtDate c d = do
  (err, es) <- partitionEithers 
                <$> scrapeEvents siteURL (dayParams d)

  forM_ err print
  runPutMany c (toDB <$> es)

parserInfo :: ParserInfo Args
parserInfo = 
  info (parseArgs <**> helper)
     (fullDesc
      <> progDesc "Scrape events data from beat.com.au"
      <> header "scape-events"
     )

parseArgs :: Parser Args
parseArgs = Args
  <$> strOption 
       (short 'c'
     <> long "db_connection"
     <> help "Connection string for database that will store results")
  <*> dateOption
       (short 's'
     <> long "start"
     <> help "start date (YYYY-MM-DD)")
  <*> (Just <$> dateOption 
                (short 'e'
              <> long "end"
              <> help "end date [optional] (YYYY-MM-DD)") 
            <|> pure Nothing)
  <*> flag False True 
       (short 'm'
     <> long "migrate"
     <> help "Enable to perform a database migration before scraping data")

dateOption :: Mod OptionFields Day -> Parser Day
dateOption = option (maybeReader parseTime)

parseTime :: String -> Maybe Day
parseTime = parseTimeM True defaultTimeLocale "%Y-%-m-%d"

mkDateRange :: Args -> [Day]
mkDateRange (Args _ s e _) = maybe [s] (enumFromTo s) e
