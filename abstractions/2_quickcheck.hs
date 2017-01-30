{-# LANGUAGE ScopedTypeVariables #-}

import Test.QuickCheck

squared x = x * x
sqrti :: Integral a => a -> a
sqrti = floor . sqrt . fromIntegral

prop_SquareRootOfNSquaredEqualsN :: Int -> Bool
prop_SquareRootOfNSquaredEqualsN n = (sqrti (squared n)) == n

prop_SqrPositive :: NonNegative Int -> Bool
prop_SqrPositive (NonNegative n) = (sqrti $ squared n) == n

main0 = quickCheck prop_SquareRootOfNSquaredEqualsN
main1 = quickCheck prop_SqrPositive

smallPositiveInteger = choose (1, 100)

prop_SqrSmallInt :: Property
prop_SqrSmallInt = forAll smallPositiveInteger $ \n ->
  (sqrti $ squared n) == (n :: Int)

main2 = quickCheck prop_SqrSmallInt

prop_SqrPositiveForGreaterEqualsZero n =
  n >= 0 ==> (sqrti $ squared n) == (n :: Int)

runTests = do
  quickCheck prop_SqrPositiveForGreaterEqualsZero

