{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}
module GigGuide.Scraper.Common where

import           Data.Char (isSpace)
import           Data.Maybe (maybeToList)
import qualified Data.List.NonEmpty as N
import           Control.Applicative (Alternative, empty)
import           Control.Monad (forM_)
import           Data.Either (partitionEithers)
import           Data.Time.Format (defaultTimeLocale, parseTimeM, ParseTime, formatTime)
import           Data.Time (Day)
import           Control.Lens ((^.))
import           Text.HTML.Scalpel (Scraper, Selector, scrapeStringLike)
import           Text.HTML.Scalpel.Class (MonadScraper, texts)
import           Network.Wreq (Options, getWith, responseBody)

import           GigGuide.Types (URL)

import qualified Data.ByteString.Lazy as B
import qualified Data.Text.Lazy as L
import qualified Data.Text      as T
import qualified Data.Text.Lazy.Encoding as LE

parseTimeDefault :: (MonadFail m, ParseTime t) => 
                    String -> String -> m t
parseTimeDefault = parseTimeM True defaultTimeLocale

parseCommaSep :: (L.Text -> a) -> L.Text -> [a]
parseCommaSep f t = f . L.strip <$> L.splitOn "," t

cleanTexts :: (MonadScraper L.Text m) => Selector -> m L.Text
cleanTexts = fmap (L.strip . joinEmpty) . texts

joinEmpty :: [L.Text] -> L.Text
joinEmpty = L.concat . filter (L.any $ not . isSpace)

optionText :: Show a => k -> Maybe a -> Maybe (k, T.Text)
optionText = optionF (T.pack . show)

optionF :: (v1 -> v2) -> k -> Maybe v1 -> Maybe (k, v2) 
optionF f k = fmap (fmap f) . option k

option :: a -> Maybe b -> Maybe (a,b)
option a = fmap ((,) a)

optionL :: Show a => k -> Maybe a -> [(k, T.Text)]
optionL k = maybeToList . optionText k

showt :: Show a => a -> T.Text
showt = T.pack . show

fmtDate :: Day -> T.Text
fmtDate = T.pack . formatTime defaultTimeLocale "%_Y-%m-%d"

runScraper :: [(URL, Options)] -> Scraper L.Text (N.NonEmpty a) -> IO [a]
runScraper urls = 
  fmap (concatMap N.toList) . scrapeAll urls

maxFetches :: Int
maxFetches = 250

scrapeAll :: [(URL, Options)] -> Scraper L.Text a -> IO [a]
scrapeAll urls scr =
  run urls
  where run []     = return []
        run ((url,o) : as) = do
          putStrLn $ "fetching page: " ++ url ++ " " ++ show o
          c <- fetchPage url o
          case scrapeStringLike (LE.decodeUtf8 c) scr of
            Just e -> (e :) <$> run as
            _      -> return []

fetchPage :: URL -> Options -> IO B.ByteString
fetchPage url opts = do
  r <- getWith opts url
  pure $ r ^. responseBody

printLefts :: Show e => IO [Either e a] -> IO [a]
printLefts a = do
  (errs,as) <- partitionEithers <$> a
  forM_ errs print
  return as

incMaybe :: Num a => Maybe a -> Maybe a
incMaybe = Just . maybe 1 (+1)

failEmpty :: Alternative m => [a] -> m (N.NonEmpty a)
failEmpty = maybe empty pure . N.nonEmpty