module GigGuide.Types.Event
  ( Event(..)
  ) where

import           Data.Text.Lazy
import           Data.Time (Day)
import           GigGuide.DB (EventCategory(..))
import           GigGuide.Types (Price, PriceRange(..), URL)

data Event = Event
  { eventName       :: Text
  , eventUrl        :: URL
  , eventDate       :: Day
  , eventCategories :: [EventCategory]
  , eventLocation   :: Text
  , eventPrice      :: Either Price PriceRange
  } deriving (Eq, Show, Ord)