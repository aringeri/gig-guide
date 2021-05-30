{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad(join)
import Data.Maybe(fromMaybe)
import System.Environment
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = do
  portEnv <- lookupEnv "PORT"
  let port = fromMaybe "8080" portEnv
  putStrLn $ "Starting beat.com.au web testing stub on port: " ++ port
  run (read port :: Int) app

staticDir :: FilePath
staticDir = "integration-test/beat-web-stub/static"

app :: Application
app request respond = respond $ case rawPathInfo request of
    "/directory" -> directory request
    _            -> notFound

notFound :: Response
notFound = responseLBS
    status404
    [("Content-Type", "text/plain")]
    "404 - Not Found"

directory :: Request -> Response
directory request = 
  let query = queryString request
      more = join $ lookup "more" query
  in case more of
    Just m  -> case m of
                 "1" -> directory1
                 "2" -> directory2
                 _   -> noMoreVenues
    Nothing -> directory1

directory1 :: Response
directory1 = responseFile
    status200
    [("Content-Type", "text/html")]
    (staticDir ++ "/directory_1.html")
    Nothing

directory2 :: Response
directory2 = responseFile
    status200
    [("Content-Type", "text/html")]
    (staticDir ++ "/directory_2.html")
    Nothing

noMoreVenues :: Response
noMoreVenues = responseFile
    status200
    [("Content-Type", "text/html")]
    (staticDir ++ "/directory-no-more-venues.html")
    Nothing
