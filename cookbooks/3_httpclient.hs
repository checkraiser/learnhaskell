{-# LANGUAGE OverloadedStrings #-}

import Network.Wreq
import Control.Lens
import Data.Maybe (fromJust)

fetchURI uri = do
  putStrLn $ "\n\n*** Fetching " ++ uri
  r <- get uri
  putStrLn $  "status code: " ++  show ( r ^. responseStatus . statusCode)
  putStrLn $ "content type: " ++ show ( r ^? responseHeader "Content-Type")
  putStrLn $ "response body: " ++ show ( fromJust $ r ^? responseBody )

main = do
  fetchURI "http://dbpedia.org/data/Sedona_Arizona.json"
  fetchURI "http://dbpedia.org/data/Sedona_Arizona.n3"
  fetchURI "http://markwatson.com"

