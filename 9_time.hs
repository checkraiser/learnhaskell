import Data.Time.Clock.POSIX
import System.TimeIt
import System.Timeout

main = do
  startingTime <- getPOSIXTime
  print startingTime
  print $ last $ take 20000001 [0..]
  endingTime <- getPOSIXTime
  print endingTime
  print (endingTime - startingTime)
