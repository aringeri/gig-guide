module GigGuide.Util
  ( readMaybeT
  , grouping
  ) where

import           Data.List (groupBy, sortOn)
import           Data.Function (on)
import           Text.Read (readMaybe)
import           Data.Text (Text, unpack)

readMaybeT :: Read a => Text -> Maybe a
readMaybeT = readMaybe . unpack

grouping :: (Eq a, Ord a) => [(a,b)] -> [(a,[b])]
grouping =
  fmap (\l -> (fst . head $ l, fmap snd l))
  . groupBy ((==) `on` fst)
  . sortOn fst