-- Find common words between two files
--
import Data.Set (fromList, toList, intersection)
import Data.Char (toLower)

fileToWords fileName = do
  fileText <- readFile fileName
  return $ fromList . words $ map toLower fileText

commonWords file1 file2 = do
  words1 <- fileToWords file1
  words2 <- fileToWords file2
  return $ toList $ intersection words1 words2

main = do
  cw <- commonWords "testfile.txt" "testfile2.txt"
  print cw
