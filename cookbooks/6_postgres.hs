{-# LANGUAGE OverloadedStrings #-}

import Database.PostgreSQL.Simple

main = do
  conn <- connect defaultConnectInfo { connectDatabase = "haskell", connectUser = "dungth", connectPassword = "123456" }
  do
    r <- query_ conn "SELECT name from customers" :: IO[(Only String)]
    print "names and emails in table 'customers' in database haskell:"
    mapM_ (print . fromOnly) r
  do
    let rows :: [(Int, String, String)]
        rows = [(4, "Truong Hoa", "truonghoa@gmail.com")]
    executeMany conn "insert into customers(id, name, email) values (?,?,?)" rows
    r2 <- query_ conn "select * from customers" :: IO [(Int, String, String)]
    print "number of rows in table 'customers':"
    print $ length r2
    print "rows in table 'customers':"
    mapM_ print r2

  close conn
