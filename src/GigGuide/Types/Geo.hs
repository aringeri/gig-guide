module GigGuide.Types.Geo
  ( Latitude(..)
  , Longitude(..)
  , Coord(..)
  , CoordBounds
  , southWest
  , northEast
  , mkBounds
) where

import Control.Monad (guard)

newtype Latitude = Latitude { getLat :: Double }
  deriving (Eq, Show, Read, Ord)

newtype Longitude = Longitude { getLon :: Double}
  deriving (Eq, Show, Read, Ord)

data Coord = Coord 
  { lat :: Latitude
  , lon :: Longitude
  } deriving (Eq, Show, Read)

data CoordBounds = CoordBounds
  { southWest :: Coord
  , northEast :: Coord
  } deriving (Eq, Show, Read)

mkBounds :: Coord -> Coord -> Maybe CoordBounds
mkBounds sw ne = do
  guard $ compareBy lat (<=) sw ne 
       && compareBy lon (<=) sw ne
  pure $ CoordBounds sw ne

compareBy :: Ord b => (a -> b) -> 
                      (b -> b -> Bool) -> 
                      a -> a -> Bool
compareBy f p a b = p (f a) (f b)
