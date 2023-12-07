{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module GigGuide.Scraper.Beat.EventDetails
( eventDetail

) where

import GigGuide.Types.EventDetails
import Data.List ( find, find )
import qualified Data.Text.Lazy as L
import Text.HTML.Scalpel
import Data.Time ( TimeOfDay, TimeOfDay )
import Control.Applicative ((<|>), optional)

import GigGuide.Scraper.Common
import GigGuide.Scraper.Beat.EventOverview (EventCreationError (..))
import Control.Monad.Logger (MonadLogger)

eventDetail :: MonadLogger m => ScraperT L.Text m (Either EventCreationError EventDetails)
eventDetail = chroot ("div" @: [hasClass "article-content"]) innerEventDetail

innerEventDetail :: MonadLogger m => ScraperT L.Text m (Either EventCreationError EventDetails)
innerEventDetail = do
  time <- orElse TimeParseError . (parseTime =<<) <$> findText "Time"
  genre <- (parseEventGenre =<<) <$> findText "Genre"
  supports <- maybe [] (L.splitOn ",") <$> findText "Support"
  vUrlItem <- orElse VenueUrlMissingError
               . fmap L.unpack <$> findItem url "LOCATION"
  ticketUrl <- optional tickets 

  return $
    EventDetails
      <$> time
      <*> pure genre
      <*> pure supports
      <*> vUrlItem
      <*> pure ticketUrl

  where
    -- findText :: MonadLogger m' => L.Text -> ScraperT L.Text m' (Maybe L.Text)
    findText = findItem textScr
    -- findItem :: MonadLogger m' => (Selector -> ScraperT L.Text m' b) 
                  -- -> L.Text -> ScraperT L.Text m' (Maybe b)
    findItem f' k = findKey k <$> metaItems f'
    findKey k = fmap snd . find ((k==) . fst)
    textScr = fmap L.strip . text
    
    tickets  = L.unpack <$> attr "href"
                                 ("div" @: [hasClass "article-sidebar"] // "a" `atDepth` 1)
    url s    = chroot s (attr "href" "a")
    orElse e = maybe (Left e) Right

parseTime :: L.Text -> Maybe TimeOfDay
parseTime t =
  let str = L.unpack t
  in      parseTimeDefault "%l.%M%p" str
      <|> parseTimeDefault "%l%p"    str


metaItems :: Monad m => (Selector -> ScraperT L.Text m b) -> ScraperT L.Text m [(L.Text, b)]
metaItems =
  chroot meta . chroots metaItem . item
  where
    meta     = "div" @: [hasClass "article-meta"]
    metaItem = "div" @: [hasClass "article-meta-item"]
    item sel = do
      k <- text $ "div" @: [hasClass "article-meta-headline"]
      v <- sel  $ "div" @: [hasClass "article-meta-content"]
      pure (k, v)

parseEventGenre :: L.Text -> Maybe EventGenre
parseEventGenre t =
  case t of
    "Rock"         -> Just Rock
    "Global"       -> Just Global
    "Jazz"         -> Just Jazz
    "Electronic"   -> Just Electronic
    "Hip Hop"      -> Just HipHop
    "Classical"    -> Just Classical
    "R&B"          -> Just RnB
    "Punk"         -> Just Punk
    "Metal"        -> Just Metal
    "Pop"          -> Just Pop
    "Acoustic"     -> Just Acoustic
    "Country/Folk" -> Just CountryFolk
    "Blues"        -> Just Blues
    "Soul/Funk"    -> Just SoulFunk
    "Experimental" -> Just Experimental
    "World Music"  -> Just WorldMusic
    "Indie"        -> Just Indie
    _              -> Nothing
