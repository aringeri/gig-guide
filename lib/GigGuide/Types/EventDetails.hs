module GigGuide.Types.EventDetails
( EventDetails(..)
, EventGenre(..)
)where
import Data.Time (TimeOfDay)
import qualified Data.Text.Lazy as L
import qualified GigGuide.Types

data EventDetails = EventDetails
  { eventTime       :: TimeOfDay
  , eventGenre      :: Maybe EventGenre
  , eventSupports   :: [L.Text]
  , eventVenueUrl   :: GigGuide.Types.URL
  , eventTicketUrl  :: Maybe GigGuide.Types.URL
  } deriving (Eq, Show, Ord)

data EventGenre =
    Rock
  | Global
  | Jazz
  | Electronic
  | HipHop
  | Classical
  | RnB
  | Punk
  | Metal
  | Pop
  | Acoustic
  | CountryFolk
  | Blues
  | SoulFunk
  | Experimental
  | WorldMusic
  | Indie
  deriving (Eq, Show, Read, Ord)
