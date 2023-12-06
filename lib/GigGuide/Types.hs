{-# LANGUAGE DeriveGeneric #-}
module GigGuide.Types
  ( Price
  , PriceRange(..)
  , URL
  , mkPriceRange
  , mkPriceRange'
  ) where

import GigGuide.Types.Range (MinMaxRange, mkRange, mkRange') 
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

type Price = Double

newtype PriceRange = PriceRange { priceRange :: MinMaxRange Price }
  deriving (Eq, Ord, Show, Read, Generic)

instance ToJSON PriceRange
instance FromJSON PriceRange

mkPriceRange :: Price -> Price -> PriceRange
mkPriceRange a = PriceRange . mkRange a

mkPriceRange' :: Price -> Price -> Maybe PriceRange
mkPriceRange' a = fmap PriceRange . mkRange' a

type URL = String