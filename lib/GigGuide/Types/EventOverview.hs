{-# LANGUAGE DeriveGeneric #-}
module GigGuide.Types.EventOverview 
( EventOverview(..)
, EventCategory(..)
) where
import GigGuide.Types(URL, Price, PriceRange)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON)
import Data.Aeson.Types (FromJSON)
import qualified Data.Text.Lazy as L
import Data.Time (Day)

data EventOverview = EventOverview
  { eventName       :: L.Text
  , eventUrl        :: URL
  , eventDate       :: Day
  , eventCategories :: [EventCategory]
  , eventLocation   :: L.Text
  , eventPrice      :: Either Price PriceRange
  } deriving (Eq, Show, Ord, Generic)

instance ToJSON EventOverview
instance FromJSON EventOverview

data EventCategory =
    Free
  | Music
  | Festival
  | Comedy
  | Theatre
  | Dance
  | Trivia
  | Markets
  | Art
  | Other L.Text
  deriving (Eq, Show, Read, Ord, Generic)

instance ToJSON EventCategory
instance FromJSON EventCategory