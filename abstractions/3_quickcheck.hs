module Debugging (runTests) where
import Test.QuickCheck
import Debug.Trace

squared :: (Num x) => x -> x
squared x = x * x

prop_TraceSqrNEqualsN :: Double -> Property
prop_TraceSqrNEqualsN n = (sqrt $ squared n) === n

prop_TraceSqrNEqualsNDebug :: Double -> Property
prop_TraceSqrNEqualsNDebug n = debugShow $ result === n
  where
    result = sqrt $ squared n
    debugShow = traceShow $ "input: " ++ show n ++ " result: " ++ show result

shouldFail = expectFailure prop_TraceSqrNEqualsN
runTests = do
  --quickCheck prop_TraceSqrNEqualsNDebug
  quickCheck shouldFail
