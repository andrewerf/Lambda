{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Test.Core.Eval
  (
  testsEval
  )
where

import Test.Core.Numerals

import Test.HUnit
import Helper

import Core.AST
import Core.Eval


assertBetaEqual :: String -> Term -> Term -> Assertion
assertBetaEqual s t1 t2 = 
  let 
    tt1 = eval t1
    tt2 = eval t2
  in
    assertEqual s tt1 tt2



testBoolean = TestLabel "boolean numerals" $ numberedTestList [
    TestCase $ assertBetaEqual "~true == false" tru_ ( mkApp2 not_ fls_ ),
    TestCase $ assertBetaEqual "~false == true" fls_ ( mkApp2 not_ tru_ ),
    TestCase $ assertBetaEqual "true & true == true" tru_ ( mkApp3 and_ tru_ tru_ ),
    TestCase $ assertBetaEqual "true & false == false" fls_ ( mkApp3 and_ tru_ fls_ ),
    TestCase $ assertBetaEqual "false & false == false" fls_ ( mkApp3 and_ fls_ fls_ ),
    TestCase $ assertBetaEqual "true | true == true" tru_ ( mkApp3 or_ tru_ tru_ ),
    TestCase $ assertBetaEqual "true | false == true" tru_ ( mkApp3 or_ tru_ fls_ ),
    TestCase $ assertBetaEqual "false | false == false" fls_ ( mkApp3 or_ fls_ fls_ )
  ]



testNatural = TestLabel "natural numerals" $ numberedTestList [
    TestCase $ assertBetaEqual "0 + 0 == 0" c0_ ( mkApp3 plus_ c0_ c0_ ),
    TestCase $ assertBetaEqual "1 + 0 == 1" c1_ ( mkApp3 plus_ c1_ c0_ ),
    TestCase $ assertBetaEqual "2 + 2 == 4" c4_ ( mkApp3 plus_ c2_ c2_ ),
    TestCase $ assertBetaEqual "2 * 2 == 4" c4_ ( mkApp3 mult_ c2_ c2_ ),
    TestCase $ assertBetaEqual "2 * 3 == 6" c6_ ( mkApp3 mult_ c2_ c3_ ),
    TestCase $ assertBetaEqual "( 3 + 4 )  * 2 == 9 + 5" ( mkApp3 plus_ c9_ c5_ ) ( mkApp3 mult_ ( mkApp3 plus_ c3_ c4_ ) c2_ ),
    TestCase $ assertBetaEqual "3 ^ 2 == 9" c9_ ( mkApp3 exp_ c3_ c2_ ),
    TestCase $ assertBetaEqual "2 ^ 4 == 9 + 7" ( mkApp3 plus_ c9_ c7_ ) ( mkApp3 exp_ c2_ c4_ )
  ]


testLetBinding = TestLabel "let-bindigns" $ numberedTestList [
    TestCase $ assertBetaEqual "(let x = 0 in x) == 0" c0_ ( mkLet c0_ loc0 ),
    TestCase $ assertBetaEqual "(let x = 2 in let y = 5 in x + y) == 7"
      c7_ ( mkLet c2_ ( mkLet c5_ ( mkApp3 plus_ loc1 loc0 ) ) )
  ]


testsEval = TestList [
    testBoolean,
    testNatural,
    testLetBinding
  ]