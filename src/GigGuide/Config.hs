{-# LANGUAGE TemplateHaskell            #-}
module GigGuide.Config where

import           Control.Lens.TH (makeClassy)

newtype DBConfig = DBConfig
  { _connStr :: String
  } deriving (Eq, Show)
makeClassy ''DBConfig

defaultDBConfig :: DBConfig
defaultDBConfig = DBConfig "host=localhost port=5454"

newtype Port = Port { _getPort :: Int }
  deriving (Eq, Show)
makeClassy ''Port

data ServerConfig = ServerConfig
  { _serverPort :: Port
  , _resourceFilePath :: FilePath
  } deriving (Eq, Show)
makeClassy ''ServerConfig

defaultServerConfig :: ServerConfig
defaultServerConfig = ServerConfig (Port 80) "."

data Environment = Environment
  { _dbConfig :: DBConfig
  , _srvConfig :: ServerConfig
  } deriving (Eq, Show)
makeClassy ''Environment

instance HasDBConfig Environment where
  dBConfig = dbConfig

instance HasServerConfig Environment where
  serverConfig = srvConfig

defaultEnvironment :: Environment
defaultEnvironment = Environment defaultDBConfig defaultServerConfig
