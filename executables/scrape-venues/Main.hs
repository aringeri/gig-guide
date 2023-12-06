module Main where

import Data.Aeson(encodeFile)
import Options.Applicative

import GigGuide.Scraper.BeatScraper ( scrapeVenues )
import GigGuide.Types (URL)

data Args = Args
 { venuesURL :: URL
 , outFile :: FilePath
 } deriving (Show, Eq)

main :: IO ()
main = do
  (Args venuesURL outFile) <- getArgs
  venues <- scrapeVenues venuesURL
  encodeFile outFile venues

getArgs :: IO Args
getArgs = 
  execParser parserInfo


parserInfo :: ParserInfo Args
parserInfo = 
  info (parseArgs <**> helper)
     (fullDesc
      <> progDesc "Scrape venue data from beat.com.au"
      <> header "scape-venues"
     )

parseArgs :: Parser Args
parseArgs = Args
  <$> strOption 
     (short 'u'
     <> long "url"
     <> value "http://www.beat.com.au/directory"
     <> help "URL to retrieve venue data. Defaults to 'http://www.beat.com.au/directory'")
  <*> strOption
     (short 'o'
     <> long "outFile"
     <> help "Output file to write the venues as JSON"
     )