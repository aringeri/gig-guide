{-# LANGUAGE DeriveGeneric #-}
module GigGuide.Types.Range
  ( MinMaxRange
  , minVal
  , maxVal
  , mkRange
  , mkRange'
  ) where
import GHC.Generics (Generic)
import Data.Aeson (ToJSON)
import Data.Aeson.Types (FromJSON)

data MinMaxRange a = MinMaxRange
  { minVal :: a
  , maxVal :: a
  } deriving (Eq, Ord, Show, Read, Generic)

instance (ToJSON a) => ToJSON (MinMaxRange a)
instance (FromJSON a) => FromJSON (MinMaxRange a)

mkRange :: Ord a => a -> a -> MinMaxRange a
mkRange a b =
  uncurry MinMaxRange $ 
    if a >= b 
      then (b, a)
      else (a, b)

-- Strict version of mkRange, expecting arguments to be ordered.
mkRange' :: Ord a => a -> a -> Maybe (MinMaxRange a)
mkRange' a b 
  | a < b     = Just $ MinMaxRange a b
  | otherwise = Nothing