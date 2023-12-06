{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import Options.Applicative
import Data.Aeson ( decodeFileStrict, encodeFile )
import GigGuide.Types.Venue (Venue)
import GigGuide.Types.VenueAndGeo (VenueAndGeo(..))
import GigGuide.Geocode.Nominatim (geocode)
import GigGuide.Types (URL)
import Control.Monad (when)
import Control.Concurrent ( threadDelay )
import Data.Traversable (for)
import Data.Maybe (isNothing, mapMaybe)
import Data.Foldable (for_)


data Args = Args
  {
    venueJsonFile :: FilePath,
    outFile :: FilePath,
    url :: URL
  } deriving(Show)

main :: IO ()
main = do
  (Args input out url) <- getArgs
  (venues :: Maybe [Venue]) <- decodeFileStrict input

  when (isNothing venues)
    (putStrLn $ "WARN: Unable to decode input file: " ++ input)

  for_ venues (\vs -> do
      let total = length vs
      putStrLn $ "Geocoding " ++ show total ++ " venues"

      gs <- for (zip [1..] vs) (\(i,v) -> do
          when (i `mod` 100 == 0)
            (putStrLn $ "processing " ++ show i ++ "/" ++ show total)
          
          c <- geocode url v
          when (isNothing c)
            (putStrLn $ "WARN: Unable to geocode venue: " ++ show v)
          threadDelay 1000000
          return (v, c)
        )

      let gs' = mapMaybe (\(v,c) -> VenueAndGeo v <$> c) gs
      encodeFile out gs'
    )

getArgs :: IO Args
getArgs =
  execParser parserInfo

parserInfo :: ParserInfo Args
parserInfo =
  info (parseArgs <**> helper)
     (fullDesc
      <> progDesc "Geocode venue addresses from beat.com.au"
      <> header "geocode-venues"
     )

parseArgs :: Parser Args
parseArgs = Args
  <$> strOption
    (short 'i'
    <> long "input-file"
    <> help "input file containing the un-geocoded venues as JSON"
    )
  <*> strOption
    (short 'o'
    <> long "outFile"
    <> help "Output file to write the geocoded venues as JSON"
    )
  <*> strOption
    (short 'u'
    <> long "url"
    <> value "https://nominatim.openstreetmap.org/search"
    <> help "the Nominatim API url: defaults to 'https://nominatim.openstreetmap.org/search'")