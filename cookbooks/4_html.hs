{-# LANGUAGE OverloadedStrings #-}

import Text.XML.HXT.Core
import Text.HandsomeSoup

main = do
  let doc = fromUrl "http://markwatson.com/"
  putStrLn "\n\n ** LINKS:\n"
  links <- runX $ doc >>> css "a" ! "href"
  mapM_ putStrLn links
  h2 <- runX $ doc >>> css "h2" ! "href"
  putStrLn "\n\n ** ALL H2 ELEMENTS::\n"
  mapM_ putStrLn h2
  allBodyText <- runX $ doc >>> css "body" //> getText
  mapM_ putStrLn allBodyText
