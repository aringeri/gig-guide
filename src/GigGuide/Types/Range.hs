module GigGuide.Types.Range
  ( MinMaxRange
  , minVal
  , maxVal
  , mkRange
  , mkRange'
  ) where

data MinMaxRange a = MinMaxRange
  { minVal :: a
  , maxVal :: a
  } deriving (Eq, Ord, Show, Read)

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