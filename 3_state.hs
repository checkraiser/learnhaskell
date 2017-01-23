import Control.Monad.State

test = do { put 3; modify (+1);  get }

main = print $ runTest test 0
  where runTest = execState
