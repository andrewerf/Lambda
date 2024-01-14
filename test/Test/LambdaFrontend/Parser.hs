{-# OPTIONS_GHC -Wno-missing-signatures #-}
module Test.LambdaFrontend.Parser where

import Test.HUnit
import Helper
import Test.Core.Numerals

import qualified Core.AST as C
import LambdaFrontend.AST
import LambdaFrontend.Parser

import Debug.Trace


checkTerm :: String -> C.Term -> Assertion
checkTerm name t =
  let
    repr = show . fromCore $ t
    t' = toCore [] . parse $ repr
  in
    assertEqualM ( name ++ " (repr: " ++ repr ++ ")" ) t t'

testBoolean = TestLabel "boolean numerals" $ numberedTestList [
    TestCase $ checkTerm "true" tru_,
    TestCase $ checkTerm "false" fls_,
    TestCase $ checkTerm "Bool" _bool_,
    TestCase $ checkTerm "&" and_,
    TestCase $ checkTerm "|" or_,
    TestCase $ checkTerm "~" not_,
    TestCase $ checkTerm "true & false" ( mkApp3 and_ tru_ fls_ )
  ]

testNatural = TestLabel "natural numerals" $ numberedTestList [
    TestCase $ checkTerm "succ" succ_,
    TestCase $ checkTerm "0" c0_,
    TestCase $ checkTerm "1" c1_,
    TestCase $ checkTerm "2" c2_,
    TestCase $ checkTerm "3" c3_,
    TestCase $ checkTerm "4" c4_,
    TestCase $ checkTerm "5" c5_,
    TestCase $ checkTerm "6" c6_,
    TestCase $ checkTerm "7" c7_,
    TestCase $ checkTerm "8" c8_,
    TestCase $ checkTerm "9" c9_,
    TestCase $ checkTerm "+" plus_,
    TestCase $ checkTerm "*" mult_,
    TestCase $ checkTerm "^" exp_
  ]


testsParser = TestList [
    testBoolean,
    testNatural
  ]
