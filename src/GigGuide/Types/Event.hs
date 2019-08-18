module GigGuide.Types.Event
  ( Event(..)
  ) where

import           Data.Text.Lazy
import           Data.Time (Day, TimeOfDay)
import           GigGuide.DB (EventCategory(..), EventGenre(..))
import           GigGuide.Types (Price, PriceRange(..), URL)

data Event = Event
  { eventName       :: Text
  , eventUrl        :: URL
  , eventDate       :: Day
  , eventCategories :: [EventCategory]
  , eventLocation   :: Text
  , eventPrice      :: Either Price PriceRange
  , eventTime       :: TimeOfDay
  , eventGenre      :: Maybe EventGenre
  , eventSupports   :: [Text]
  , eventVenueUrl   :: URL
  , eventTicketUrl  :: Maybe URL
  } deriving (Eq, Show, Ord)