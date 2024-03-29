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


testSimple = TestLabel "simple lifts" $ numberedTestList [
    TestCase $ assertEqualM "typeof(\\X : * . X) == Πx:*.* ( * -> * )"
      ( mkPi TmStar TmStar )
      ( lift0 $ mkAbs TmStar loc0 ),
    TestCase $ assertEqualM "typeof(\\X : *. \\x : X. x) == ΠX:*.X -> X == ΠX:*.Πx:X.X"
      ( mkPi TmStar ( mkArrow2 loc0 loc0 ) )
      ( lift0 $ mkAbs TmStar ( mkAbs loc0 loc0 ) )
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

testLetBinding = TestLabel "let-bindigns" $ numberedTestList [
    TestCase $ assertEqualM "typeof(let x = 0 in x) == Nat" _nat_ ( lift0 $ mkLet c0_ loc0 ),
    TestCase $ assertEqualM "typeof(let x = 2 in let y = 5 in x + y) == Nat"
      _nat_ ( lift0 $ mkLet c2_ ( mkLet c5_ ( mkApp3 plus_ loc1 loc0 ) ) )
  ]


testPairs = TestLabel "pair" $ numberedTestList [
    TestCase $ assertEqualM "typeof(fst) == @X:*.@Y:*. Pair X Y -> X"
        ( mkPi TmStar $ mkPi TmStar $ mkArrow2 ( mkApp3 _pair_ loc1 loc0 ) loc1 )
        ( lift0 fst_ ),
    TestCase $ assertEqualM "forall X:*, Y:*, x:X, y:Y. typeof(fst(pair(x, y)) == X"
        ( mkPi TmStar $ mkPi TmStar $ mkPi loc1 $ mkPi loc1 loc3 )
        ( lift0 $ mkAbs TmStar $ mkAbs TmStar $ mkAbs loc1 $ mkAbs loc1 $ mkApp4 fst_ loc3 loc2 ( mkApp5 pair_ loc3 loc2 loc1 loc0 ) ),
    TestCase $ assertEqualM "forall X:*, Y:*, x:X, y:Y. typeof(snd(pair(x, y)) == y"
        ( mkPi TmStar $ mkPi TmStar $ mkPi loc1 $ mkPi loc1 loc2 )
        ( lift0 $ mkAbs TmStar $ mkAbs TmStar $ mkAbs loc1 $ mkAbs loc1 $ mkApp4 snd_ loc3 loc2 ( mkApp5 pair_ loc3 loc2 loc1 loc0 ) )
  ]


testsTyping = TestList [
    testSimple,
    testBoolean,
    testNatural,
    testLetBinding,
    testPairs
  ]