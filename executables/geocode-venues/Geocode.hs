{-# LANGUAGE OverloadedStrings          #-}
module Geocode
  ( ConnStr(..)
  , Config(..)
  , ApiKey(..)
  , removeEsc
  ) where

import           Data.Text

removeEsc :: Text -> Text
removeEsc = replace "\n" " " .
            replace "\t" " " .
            replace "\r" " "

newtype ConnStr = ConnStr
  { unConnStr :: Text }
  deriving (Show, Eq)

newtype ApiKey = ApiKey
  { unApiKey :: Text }
  deriving (Show, Eq)

data Config = Config
  { dbConn :: ConnStr
  , apiKey :: ApiKey
  } deriving (Show, Eq)
