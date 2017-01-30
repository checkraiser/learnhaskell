import Control.Monad.State

incrementState :: State Int Int
incrementState = do
  n <- get
  put $ n + 1
  return n

bumpVals (a, b) = (a+1, b+2)

main = do
  print $ runState incrementState 1
