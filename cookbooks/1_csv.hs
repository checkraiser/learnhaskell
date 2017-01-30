import Text.CSV (parseCSVFromFile, CSV)
import Data.Either.Unwrap (fromRight)

readCSVFile :: FilePath -> IO CSV
readCSVFile filename = do
  c <- parseCSVFromFile filename
  return $ fromRight c

main = do
  c <- readCSVFile "test.csv"
  let header:rows = c
  print header
