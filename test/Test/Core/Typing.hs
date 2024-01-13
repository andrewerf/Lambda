{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Test.Core.Typing
  (
  testsTyping
  )
where

import Test.Core.Numerals

import Test.HUnit
import Helper

import Core.AST
import Core.Typing


assertEqualM :: ( Monad m, Show ( m a ), Eq ( m a ) ) => String -> a -> m a -> Assertion
assertEqualM s y = assertEqual s ( return y )


testSimple = TestLabel "simple lifts" $ numberedTestList [
    TestCase $ assertEqualM "typeof(\\X : * . X) == Πx:*.* ( * -> * )"
      ( TmPi TmStar TmStar )
      ( lift0 $ TmAbs TmStar loc0 ),
    TestCase $ assertEqualM "typeof(\\X : *. \\x : X. x) == ΠX:*.X -> X == ΠX:*.Πx:X.X"
      ( TmPi TmStar ( mkArrow2 loc0 loc0 ) )
      ( lift0 $ TmAbs TmStar ( TmAbs loc0 loc0 ) )
  ]


testBoolean = TestLabel "boolean numerals" $ numberedTestList [
    TestCase $ assertEqualM "typeof(true) == Bool" _bool_ ( lift0 tru_ ),
    TestCase $ assertEqualM "typeof(false) == Bool" _bool_ ( lift0 fls_ ),
    TestCase $ assertEqualM "typeof(true & false) == Bool" _bool_ ( lift0 ( mkApp3 and_ tru_ fls_ ) ),
    TestCase $ assertEqualM "typeof(true & false & false) == Bool" _bool_ ( lift0 ( mkApp3 and_ ( mkApp3 and_ tru_ fls_ ) fls_ ) )
  ]

testNatural = TestLabel "natural numerals" $ numberedTestList [
    TestCase $ assertEqualM "typeof(0) == Nat" _nat_ ( lift0 c0_ ),
    TestCase $ assertEqualM "typeof(1) == Nat" _nat_ ( lift0 c1_ ),
    TestCase $ assertEqualM "typeof(1 + 1) == Nat" _nat_ ( lift0 ( mkApp3 plus_ c1_ c1_ ) ),
    TestCase $ assertEqualM "typeof(5 + 7) == Nat" _nat_ ( lift0 ( mkApp3 plus_ c5_ c7_ ) ),
    TestCase $ assertEqualM "typeof(4 * 8) == Nat" _nat_ ( lift0 ( mkApp3 mult_ c4_ c8_ ) )
  ]


testsTyping = TestList [
    testSimple,
    testBoolean,
    testNatural
  ]