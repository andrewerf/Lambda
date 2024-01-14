module Helper
  (
  numberedTestList,
  assertEqualM
  )
where

import Test.HUnit

numberedTestList :: [Test] -> Test
numberedTestList = TestList . go 0
  where
    go :: Int -> [Test] -> [Test]
    go i ( test : tests ) = TestLabel ( show i ) test : go ( i + 1 ) tests
    go _ _ = []
    

assertEqualM :: ( Monad m, Show ( m a ), Eq ( m a ) ) => String -> a -> m a -> Assertion
assertEqualM s y = assertEqual s ( return y )
