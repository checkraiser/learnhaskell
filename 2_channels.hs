import Control.Concurrent
import Control.Concurrent.STM

main = do
  channel1 <- atomically newTQueue
  channel2 <- atomically newTQueue

  forkIO $ atomically $ writeTQueue channel1 "hello"
  forkIO $ atomically $ writeTQueue channel2 "world"

  hello <- atomically $ readTQueue channel1
  world <- atomically $ readTQueue channel2

  putStrLn (hello ++ world)
