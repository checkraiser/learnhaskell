{-# LANGUAGE DeriveDataTypeable #-}

import Text.JSON.Generic

data Person = Person {
  name :: String,
  email :: String
} deriving (Show, Data, Typeable)

main = do
  let a = encodeJSON $ Person "Dung" "dungth@hpu.edu.vn"
  print a
  let d = (decodeJSON a :: Person)
  print d
