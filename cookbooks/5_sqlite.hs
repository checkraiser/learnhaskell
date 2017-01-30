{-# LANGUAGE OverloadedStrings #-}

import Database.SQLite.Simple

main = do
  conn <- open "test.db"
  do
    r <- query_ conn
        "SELECT name FROM sqlite_master WHERE type='table'" :: IO[Only String]
    print "Table names in database test.db:"
    mapM_ (print . fromOnly) r
