{-# LANGUAGE TemplateHaskell            #-}
module GigGuide.DB.EventGenre where

import           Database.Persist.TH

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
derivePersistField "EventGenre"