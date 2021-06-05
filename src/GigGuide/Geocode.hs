{-# LANGUAGE OverloadedStrings          #-}
module GigGuide.Geocode
  ( ConnStr(..)
  , Config(..)
  , ApiKey(..)
  , Url
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

type Url = String

data Config = Config
  { dbConn :: ConnStr
  , apiKey :: ApiKey
  , geocoderEndpoint :: Url
  } deriving (Show, Eq)
