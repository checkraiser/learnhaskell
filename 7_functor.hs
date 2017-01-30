fileToWords fileName = do
  fileText <- readFile fileName
  return $ words fileText

main = do
  words1 <- fileToWords "testfile.txt"
  print $ reverse words1
  words2 <- reverse <$> fileToWords "testfile.txt"
  print words2
